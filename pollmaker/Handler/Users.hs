module Handler.Users where

import Import
import Data.List.Split
import Util.Util 

getUsersR :: Handler Html
getUsersR = defaultLayout $ do
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    r <- liftIO readCodes
    $(widgetFile "users")

codes :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => FormInput m Text 
codes = ireq textField "thecodes" 

postUsersR :: Handler Html
postUsersR = do
        result <- (splitOn "\r\n" . unpack) <$> runInputPost codes
        liftIO $ saveData result codesFile
        defaultLayout [whamlet|<p>#{show result}|]



