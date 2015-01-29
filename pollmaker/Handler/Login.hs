
module Handler.Login where

import Import
import Util.Util

code :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m Text 
code = ireq textField "usercode" 

postLoginR:: Handler Html
postLoginR = do
        userCode <- runInputPost code
        allCodes <- liftIO readCodes
        admin <- liftIO $ isAdmin userCode
        case userCode `elem` allCodes || admin of
             True -> setMessage "OK" >> (setSessionCode userCode)
             False -> setMessage "Nope"
        redirect HomeR

getLoginR :: Handler Html
getLoginR = loginPage

loginPage :: Handler Html
loginPage = defaultLayout $ do 
        setTitle "Please login"
        $(widgetFile "login")

getLogoutR :: Handler Html
getLogoutR = defaultLayout $ logout >> redirect HomeR
