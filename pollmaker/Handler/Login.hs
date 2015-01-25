module Handler.Login where

import Import
import Util.Util

code :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m Text 
code = ireq textField "usercode" 

postLoginR:: Handler Html
postLoginR = do
        userCode <- runInputPost code
        allCodes <- liftIO readCodes
        case userCode `elem` allCodes of
             True -> setMessage "OK" >> (setSession "logged" userCode)
             False -> setMessage "Nope"
        redirect HomeR 

getLoginR :: Handler Html
getLoginR = loginPage

loginPage :: Handler Html
loginPage = defaultLayout $ do 
        setTitle "Please login"
        $(widgetFile "login")

