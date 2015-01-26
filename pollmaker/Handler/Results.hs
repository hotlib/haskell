module Handler.Results where

import Import
import Handler.Survey
import Util.Util

getResultsR :: Handler Html
getResultsR = resultsPage

resultsPage :: Handler Html
resultsPage = defaultLayout $ do 
  res <- liftIO $ ((readLines answersFile ) :: IO ([SurveyAnswers]))
  setTitle "The results"
  $(widgetFile "results")

