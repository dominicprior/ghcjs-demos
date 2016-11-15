module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar, newMVar)

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

main = run 3708 $ do
    state <- newMVar (100,100)
    paint state
    Just doc <- currentDocument
    on doc D.click $ do
        (x, y) <- mouseClientXY
        liftIO $ takeMVar state
        liftIO $ putMVar state (x,y)
        liftIO $ paint state
        return ()

    syncPoint

    forever $ do
        paint state
        (x,y) <- takeMVar state
        putMVar state (x+1, y+1)
        threadDelay 50000

paint state = do
    (x,y) <- takeMVar state
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body $ Just $
        "<svg id='s' width=\"800\" height=\"600\">" ++
        "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++
                 "\" r=\"40\" stroke=\"green\" stroke-width=\"4\" fill=\"pink\" />" ++
        "</svg>"
    putMVar state (x,y)
