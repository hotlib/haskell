module Handler.Results where

import Import

getResultsR :: Handler Html
getResultsR = resultsPage

resultsPage :: Handler Html
resultsPage = defaultLayout $ do 
        setTitle "The results"
        $(widgetFile "results")

