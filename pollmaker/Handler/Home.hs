module Handler.Home where

import Import

loadHomepage :: Handler Html
loadHomepage = defaultLayout $ do 
    aDomId <- newIdent
    setTitle "Editing the Survey"
    $(widgetFile "homepage")


getHomeR :: Handler Html
getHomeR = do
  x <- lookupSession "logged"
  case x of 
       Just y -> loadHomepage
       Nothing -> redirect LoginR
