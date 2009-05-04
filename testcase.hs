import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception

thing1 state = do
  inchan <- newChan
  chlist <- takeMVar state
  putMVar state (inchan : chlist)
  result <- readChan inchan
  putStrLn $ "got " ++ result

thing2 state = do
  chlist <- takeMVar state
  case chlist of
    [] ->
        putMVar state []
    (outchan : chlist') -> do
        putMVar state chlist'
        writeChan outchan "hello"
        putStrLn "sent hello"

main = do
  state <- newMVar []
  forever $ do
    forkIO $ thing2 state
    forkIO $ thing1 state
