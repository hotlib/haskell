module Handler.Survey where

import Import
import System.IO

data Survey = Survey {name :: String, questions :: [Question]} deriving (Show)  
type Question = Text 

instance ToJSON Survey where
    toJSON Survey{..} = object
        [ "name" .= name,
          "questions" .= questions 
        ]

getSurveyR :: Handler Html
getSurveyR = do
   defaultLayout $ do
    let greeting = "Say \"Hello\"" :: String
        s = Survey { name = "test survey", questions = ["aaa", "bbb", "ccc"] } 
    aDomId <- newIdent
    setTitle "survey"
    $(widgetFile "survey")

postCreateSurveyR :: Handler Html
postCreateSurveyR = do
  (postData, _) <- runRequestBody
  let qs = map snd postData
  let s = Survey { name = "test survey", questions = qs }
  liftIO $ do 
    outh <- openFile "data.txt" WriteMode
    hPrint outh s
    hClose outh 
  defaultLayout [whamlet|<p>#{show s}|]
