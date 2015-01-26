module Handler.Home where

import Import
import Util.Util

loadHomepage :: Handler Html
loadHomepage = defaultLayout $ do 
    aDomId <- newIdent
    setTitle "homepage"
    $(widgetFile "homepage")

getHomeR :: Handler Html
getHomeR = authCode (redirect SurveyR) loadHomepage
