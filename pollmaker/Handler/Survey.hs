module Handler.Survey where

import Import
import System.IO
import Prelude (read)

data Survey = Survey {name :: String, questions :: [Question]} deriving (Show, Read)  
type Question = Text 

instance ToJSON Survey where
    toJSON Survey{..} = object
        [ "name" .= name,
          "questions" .= questions 
        ]

getData :: IO Survey
getData = do
  outh <- openFile "data.txt" ReadMode
  s <- (read <$> System.IO.hGetContents outh )
  s `seq` hClose outh
  return s

saveData :: Survey -> IO ()
saveData s =  do 
  outh <- openFile "data.txt" WriteMode
  hPrint outh s
  hClose outh 

getSurveyR :: Handler Html
getSurveyR = do
  defaultLayout $ do
    s <- liftIO getData
    setTitle "Survey"
    $(widgetFile "survey")

postCreateSurveyR :: Handler Html
postCreateSurveyR = do
  (postData, _) <- runRequestBody
  let qs = map snd postData
  let s = Survey { name = "test survey", questions = qs }
  liftIO $ saveData s
  defaultLayout [whamlet|<p>#{show s}|]
