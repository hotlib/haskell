
module Handler.Settings where

import Import

loadSettings :: Handler Html
loadSettings = defaultLayout $ do 
    aDomId <- newIdent
    setTitle "Settings"
    $(widgetFile "settings")


getSettingsR :: Handler Html
getSettingsR = do
  x <- lookupSession "logged"
  case x of 
       Just y -> loadSettings
       Nothing -> redirect LoginR


postSettingsR :: Handler Html
postSettingsR = do
  x <- lookupSession "logged"
  case x of 
       Just y -> loadSettings
       Nothing -> redirect LoginR
