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
    state <- newMVar (0,0)
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body $ Just $
        "<h1 id='hi'>Kia ora 5(Hi)</h1>" ++
        "<svg id='s' width=\"200\" height=\"200\">" ++
        "<circle cx=\"50\" cy=\"50\" r=\"40\" stroke=\"green\" stroke-width=\"4\" fill=\"yellow\" />" ++
        "</svg>"
    on doc D.click $ do
        (x, y) <- mouseClientXY
        liftIO $ takeMVar state
        liftIO $ putMVar state (x,y)
        moveHi doc x y
        return ()

    syncPoint

    forever $ do
        (x,y) <- takeMVar state
        moveHi doc x y
        putMVar state (x+1, y+1)
        threadDelay 50000
            

moveHi doc x y = do
    Just elt <- getElementById doc "s"
    setInnerHTML elt $ Just $
        "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"40\" stroke=\"green\" stroke-width=\"4\" fill=\"pink\" />"
