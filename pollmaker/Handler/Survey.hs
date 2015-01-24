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

getD :: IO Survey
getD = do
        s <- getData dataFile 
        case s of
            Just x -> return x
	    Nothing -> return $ Survey {name = "empty survey", questions = [] }

getSurveyR :: Handler Html
getSurveyR = do
  defaultLayout $ do
    s <- liftIO getD -- $ getData dataFile 
    setTitle "Survey"
    $(widgetFile "survey")

postCreateSurveyR :: Handler Html
postCreateSurveyR = do
  (postData, _) <- runRequestBody
  let qs = map snd postData
  let s = Survey { name = "test survey", questions = qs }
  liftIO $ saveData s dataFile
  defaultLayout [whamlet|<p>#{show s}|]
