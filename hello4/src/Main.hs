module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

import GHCJS.DOM (run, syncPoint, currentDocument)
import GHCJS.DOM.Document (getBody, createElement, createTextNode
    , getElementById, createAttribute)
import GHCJS.DOM.Element (setInnerHTML, setAttribute)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import qualified GHCJS.DOM.Document as D (click)
import qualified GHCJS.DOM.Element as E (click)
import GHCJS.DOM.WindowTimers (WindowTimers, setInterval)
--import GHCJS.DOM.Types (WindowTimers)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO ()

--k :: Bool
--k = on

--setInterval :: (MonadIO m) => WindowTimers -> JSVal -> Int -> m Int
--newtype WindowTimers = WindowTimers { unWindowTimers :: JSVal }


main = run 3708 $ do
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body (Just "<h1 id='hi'>Kia ora 4(Hi)</h1>")
    on doc D.click $ do
        (x, y) <- mouseClientXY
        moveHi doc x y
        Just newParagraph <- createElement doc (Just "p")
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    Just exit <- createElement doc (Just "span")
    text <- createTextNode doc "Click here to exit"
    appendChild exit text
    appendChild body (Just exit)
    on exit E.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    forever $ do
        threadDelay 2000000
        moveHi doc 200 200
            

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    setInnerHTML body (Just "<h1>Ka kite ano (See you later)</h1>")
    return ()


--moveHi :: Int -> Int -> IO ()
moveHi doc x y = do
    Just elt <- getElementById doc "hi"
    setAttribute elt "style" $
        "position:relative; color:red; top:" ++
        show y ++ "px; left:" ++ show x ++ "px;"
