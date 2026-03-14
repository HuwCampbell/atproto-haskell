-- | HTML views for the Statusphere example app, using Lucid.
module Statusphere.Views
  ( homePage
  , loginPage
  , errorPage
  , statusOptions
  ) where

import           Lucid
import qualified Data.Text               as T

import           Statusphere.Database     (StatusRow (..))

-- | The available status emoji options.
statusOptions :: [T.Text]
statusOptions =
  [ "👍", "👎", "💙", "🥹", "😧", "🙃", "😉", "😎"
  , "🤓", "🤨", "🥳", "😭", "😤", "🤯", "🫡", "💀"
  , "✊", "🤘", "👀", "🧠", "👩\x200D💻", "🧑\x200D💻", "🥷", "🧌", "🦋", "🚀"
  ]

-- | The page shell (wraps content in <html>).
shell :: T.Text -> Html () -> Html ()
shell title content = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/public/styles.css"]
    meta_ [charset_ "utf-8"]
  body_ content

-- | The home page view.
homePage
  :: [StatusRow]         -- ^ Recent statuses
  -> Maybe T.Text        -- ^ Display name of the logged-in user (Nothing = logged out)
  -> Maybe StatusRow     -- ^ Current user's status
  -> Html ()
homePage statuses mDisplayName myStatus =
  shell "Statusphere" $ do
    div_ [id_ "root"] $ do
      div_ [id_ "header"] $ do
        h1_ "Statusphere"
        p_ "Set your status on the Atmosphere."

      div_ [class_ "container"] $ do
        -- Session card
        div_ [class_ "card"] $
          case mDisplayName of
            Just name ->
              form_ [action_ "/logout", method_ "post", class_ "session-form"] $ do
                div_ $ do
                  "Hi, "
                  strong_ (toHtml name)
                  ". What's your status today?"
                div_ $
                  button_ [type_ "submit"] "Log out"

            Nothing ->
              div_ [class_ "session-form"] $ do
                div_ $ do
                  a_ [href_ "/login"] "Log in"
                  " to set your status!"
                div_ $
                  a_ [href_ "/login", class_ "button"] "Log in"

        -- Status picker (only useful when logged in)
        form_ [action_ "/status", method_ "post", class_ "status-options"] $
          mapM_ (statusButton myStatus) statusOptions

        -- Status feed
        mapM_ (uncurry statusLine) (zip [0 :: Int ..] statuses)

-- | A single status emoji button.
statusButton :: Maybe StatusRow -> T.Text -> Html ()
statusButton myStatus emoji =
  let selected = maybe False (\s -> srStatus s == emoji) myStatus
      cls      = if selected then "status-option selected" else "status-option"
  in  button_
        [ class_ cls
        , name_ "status"
        , value_ emoji
        ]
        (toHtml emoji)

-- | A single status line in the feed.
statusLine :: Int -> StatusRow -> Html ()
statusLine idx row =
  let cls = if idx == 0 then "status-line no-line" else "status-line"
      handle = srAuthorDid row  -- We use the DID directly as we don't resolve handles here
      bskyLink = "https://bsky.app/profile/" <> handle
  in  div_ [class_ cls] $ do
        div_ $
          div_ [class_ "status"] (toHtml (srStatus row))
        div_ [class_ "desc"] $ do
          a_ [class_ "author", href_ bskyLink] (toHtml ("@" <> handle))
          toHtml (" is feeling " <> srStatus row)

-- | The login page view.
loginPage :: Maybe T.Text -> Html ()
loginPage mError =
  shell "Log in – Statusphere" $ do
    div_ [id_ "root"] $ do
      div_ [id_ "header"] $ do
        h1_ "Statusphere"
        p_ "Set your status on the Atmosphere."

      div_ [class_ "container"] $ do
        form_ [action_ "/login", method_ "post", class_ "login-form"] $ do
          input_
            [ type_ "text"
            , name_ "handle"
            , placeholder_ "Enter your handle (eg alice.bsky.social)"
            , required_ ""
            ]
          button_ [type_ "submit"] "Log in"

        case mError of
          Just err -> p_ (toHtml ("Error: " <> err))
          Nothing  -> return ()

-- | A simple error page.
errorPage :: T.Text -> Html ()
errorPage msg =
  shell "Error – Statusphere" $
    div_ [id_ "root"] $
      div_ [class_ "container"] $
        h1_ (toHtml msg)
