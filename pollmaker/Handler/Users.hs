module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "users")
