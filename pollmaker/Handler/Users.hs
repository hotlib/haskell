module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
    defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "users")
