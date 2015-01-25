module Handler.Survey where

import Import
import Util.Util

data Survey = Survey {name :: String, questions :: [Question]} deriving (Show, Read)  
type Question = Text 

instance ToJSON Survey where
    toJSON Survey{..} = object
        [ "name" .= name,
          "questions" .= questions 
        ]

dataFile :: String
dataFile = "data.txt"

readSurvey :: IO Survey
readSurvey = do
        s <- getData dataFile 
        case s of
            Just x -> return x
            Nothing -> return $ Survey {name = "empty survey", questions = [] }

surveyPage :: Handler Html
surveyPage = defaultLayout $ do 
       s <- liftIO readSurvey 
       setTitle "Survey"
       $(widgetFile "survey")

getSurveyR :: Handler Html
getSurveyR = do
  x <- lookupSession "logged"
  case x of 
       Just y -> surveyPage 
       Nothing -> redirect LoginR

postCreateSurveyR :: Handler Html
postCreateSurveyR = do
  (postData, _) <- runRequestBody
  let qs = map snd postData
      s = Survey { name = "test survey", questions = qs }
  liftIO $ saveData s dataFile
  defaultLayout [whamlet|<p>#{show s}|]
