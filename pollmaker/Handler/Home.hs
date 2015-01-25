module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  x <- lookupSession "logged"
  case x of 
       Just y -> redirect SurveyR 
       Nothing -> redirect LoginR
