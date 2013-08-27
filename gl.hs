import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import Data.IORef
import GHC.IO

type Healh = Float
newtype XYPosition = XYPosition { getXYPosition :: (GLfloat, GLfloat) } deriving (Show)
data Soldier = Soldier { status :: (Healh, XYPosition) }

newtype Everything = Everything { soldiers :: [Soldier] }

main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Moving soldiers"
  initEverythingIORef <- initEverything
  displayCallback $= (display initEverythingIORef)
  addTimerCallback 100 (updateCallback initEverythingIORef) 
  mainLoop


initEverything :: IO (IORef Everything)
initEverything = newIORef $ Everything [Soldier (1, XYPosition (0.1, 0.1)), Soldier (1, XYPosition (0.4, 0.4)) ]
                 
updateSoldier :: Soldier -> Soldier
updateSoldier (Soldier (health, position)) = Soldier (health, newXYPosition)
                  where newXYPosition = let (x, y) = getXYPosition position in XYPosition (newPos x, newPos y)
                        newPos a = (unsafePerformIO (randomRIO(0.1::GLfloat,0.9::GLfloat)))


updateSoldiers :: [Soldier] -> [Soldier]
updateSoldiers x = foldl (\a soldier -> (updateSoldier soldier) : a) [] x

changeEverything :: IORef Everything -> IO ()                 
changeEverything everythingIoref = do
                      e <- readIORef everythingIoref 
                      let newEverything = e { soldiers = (updateSoldiers $ soldiers e)}
                      writeIORef everythingIoref newEverything
                      return ()
                            

getSoldierPositions :: Everything -> [(GLfloat, GLfloat)]
getSoldierPositions e = foldl (\a soldier -> (getXYPosition . snd . status $ soldier) : a) [] $ soldiers e


updateCallback :: IORef Everything -> IO ()
updateCallback everything = do 
          postRedisplay Nothing
          changeEverything everything
          addTimerCallback 100 (updateCallback everything) 

display :: IORef Everything -> IO ()
display everything = do
  
  newEverything <- readIORef everything
 
  clear [ColorBuffer]
  pointSize $= 10  
  renderPrimitive Points $ do
    renderPrimitive Points $ mapM_ (\(x, y)->vertex $ Vertex2 x y) $ getSoldierPositions newEverything
    color $ soldierColor
  flush
  swapBuffers
  where soldierColor = let a = 0.3 :: GLfloat
                           b = 0.7 :: GLfloat 
                       in
                           Color4 a b a b 

