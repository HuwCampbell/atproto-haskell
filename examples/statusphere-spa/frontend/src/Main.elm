module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E
import Task
import Time


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type alias StatusEntry =
    { uri : String
    , authorDid : String
    , status : String
    , createdAt : String
    , indexedAt : String
    }

type alias Model =
    { statuses : List StatusEntry
    , myDid : Maybe String
    , selectedStatus : Maybe String
    , handleInput : String
    , loading : Bool
    , error : Maybe String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { statuses = []
      , myDid = Nothing
      , selectedStatus = Nothing
      , handleInput = ""
      , loading = True
      , error = Nothing
      }
    , Cmd.batch [ getSession, getStatuses ]
    )


-- UPDATE

type Msg
    = GotStatuses (Result Http.Error (List StatusEntry))
    | GotSession (Result Http.Error (Maybe String))
    | SetStatus String
    | StatusSet (Result Http.Error ())
    | UpdateHandle String
    | SubmitLogin
    | Logout
    | LogoutDone (Result Http.Error ())
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStatuses (Ok entries) ->
            ( { model | statuses = entries, loading = False, error = Nothing }, Cmd.none )

        GotStatuses (Err _) ->
            ( { model | loading = False, error = Just "Failed to load statuses" }, Cmd.none )

        GotSession (Ok did) ->
            ( { model | myDid = did }, Cmd.none )

        GotSession (Err _) ->
            ( model, Cmd.none )

        SetStatus emoji ->
            ( { model | selectedStatus = Just emoji }
            , setStatus emoji
            )

        StatusSet (Ok _) ->
            ( model, getStatuses )

        StatusSet (Err _) ->
            ( { model | error = Just "Failed to set status" }, Cmd.none )

        UpdateHandle h ->
            ( { model | handleInput = h }, Cmd.none )

        SubmitLogin ->
            -- Login is a form POST that triggers a server-side redirect.
            -- We use Nav.load to navigate to a URL that will submit the login.
            ( model, Nav.load ("/login?handle=" ++ model.handleInput) )

        Logout ->
            ( model, doLogout )

        LogoutDone _ ->
            ( { model | myDid = Nothing, selectedStatus = Nothing }, Cmd.none )

        Tick _ ->
            ( model, getStatuses )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 5000 Tick


-- HTTP

getStatuses : Cmd Msg
getStatuses =
    Http.get
        { url = "/xrpc/xyz.statusphere.getStatuses"
        , expect = Http.expectJson GotStatuses statusesDecoder
        }

statusesDecoder : D.Decoder (List StatusEntry)
statusesDecoder =
    D.field "statuses" (D.list statusEntryDecoder)

statusEntryDecoder : D.Decoder StatusEntry
statusEntryDecoder =
    D.map5 StatusEntry
        (D.field "uri" D.string)
        (D.field "authorDid" D.string)
        (D.field "status" D.string)
        (D.field "createdAt" D.string)
        (D.field "indexedAt" D.string)

getSession : Cmd Msg
getSession =
    Http.get
        { url = "/xrpc/xyz.statusphere.getSession"
        , expect = Http.expectJson GotSession sessionDecoder
        }

sessionDecoder : D.Decoder (Maybe String)
sessionDecoder =
    D.field "did" (D.nullable D.string)

setStatus : String -> Cmd Msg
setStatus emoji =
    Http.post
        { url = "/xrpc/xyz.statusphere.setStatus"
        , body = Http.jsonBody (E.object [ ( "status", E.string emoji ) ])
        , expect = Http.expectWhatever StatusSet
        }

doLogout : Cmd Msg
doLogout =
    Http.post
        { url = "/logout"
        , body = Http.emptyBody
        , expect = Http.expectWhatever LogoutDone
        }


-- VIEW

statusOptions : List String
statusOptions =
    [ "👍", "👎", "💙", "🥹", "😧", "🙃", "😉", "😎"
    , "🤓", "🤨", "🥳", "😭", "😤", "🤯", "🫡", "💀"
    , "✊", "🤘", "👀", "🧠", "🥷", "🧌", "🦋", "🚀"
    ]

view : Model -> Html Msg
view model =
    div [ id "root" ]
        [ viewHeader
        , div [ class "container" ]
            [ viewSessionCard model
            , viewStatusPicker model
            , viewError model
            , viewStatuses model
            ]
        ]

viewHeader : Html Msg
viewHeader =
    div [ id "header" ]
        [ h1 [] [ text "Statusphere" ]
        , p [] [ text "Set your status on the Atmosphere." ]
        ]

viewSessionCard : Model -> Html Msg
viewSessionCard model =
    div [ class "card" ] <|
        case model.myDid of
            Just did ->
                [ div [ class "session-form" ]
                    [ div []
                        [ text "Hi, "
                        , strong [] [ text did ]
                        , text ". What's your status today?"
                        ]
                    , div []
                        [ button [ onClick Logout ] [ text "Log out" ] ]
                    ]
                ]

            Nothing ->
                [ div [ class "session-form" ]
                    [ div []
                        [ text "Log in to set your status!" ]
                    , div []
                        [ Html.form [ class "login-inline", onSubmit SubmitLogin ]
                            [ input
                                [ type_ "text"
                                , placeholder "your-handle.bsky.social"
                                , value model.handleInput
                                , onInput UpdateHandle
                                ]
                                []
                            , button [ type_ "submit" ] [ text "Log in" ]
                            ]
                        ]
                    ]
                ]

viewStatusPicker : Model -> Html Msg
viewStatusPicker model =
    div [ class "status-options" ] <|
        List.map (viewStatusButton model) statusOptions

viewStatusButton : Model -> String -> Html Msg
viewStatusButton model emoji =
    let
        isSelected =
            model.selectedStatus == Just emoji

        cls =
            if isSelected then
                "status-option selected"
            else
                "status-option"
    in
    button
        [ class cls
        , onClick (SetStatus emoji)
        , disabled (model.myDid == Nothing)
        ]
        [ text emoji ]

viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just err ->
            p [ class "error visible" ] [ text err ]

        Nothing ->
            text ""

viewStatuses : Model -> Html Msg
viewStatuses model =
    if model.loading then
        p [] [ text "Loading..." ]
    else
        div [] <|
            List.indexedMap viewStatusLine model.statuses

viewStatusLine : Int -> StatusEntry -> Html Msg
viewStatusLine idx entry =
    let
        cls =
            if idx == 0 then
                "status-line no-line"
            else
                "status-line"

        bskyLink =
            "https://bsky.app/profile/" ++ entry.authorDid
    in
    div [ class cls ]
        [ div []
            [ div [ class "status" ] [ text entry.status ] ]
        , div [ class "desc" ]
            [ a [ class "author", href bskyLink ] [ text ("@" ++ entry.authorDid) ]
            , text (" is feeling " ++ entry.status)
            ]
        ]
