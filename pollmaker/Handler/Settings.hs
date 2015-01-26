
module Handler.Settings where

import Import
import Util.Util

getPassword :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m Text 
getPassword = ireq textField "adminpasswd" 

loadSettings :: Handler Html
loadSettings = defaultLayout $ do 
    aDomId <- newIdent
    setTitle "Settings"
    $(widgetFile "settings")

updatePassword :: Handler Html
updatePassword = do 
       p <- runInputPost getPassword
       liftIO $ writeAdminPassword p 
       setSessionCode p
       redirect HomeR

getSettingsR :: Handler Html
getSettingsR = authCode (redirect HomeR) loadSettings

postSettingsR :: Handler Html
postSettingsR = authCode (redirect HomeR) updatePassword


