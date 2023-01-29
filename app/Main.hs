module Main (main) where

import Control.Monad.State ( evalStateT )
import Control.Monad ()
import Lib 

initGameState :: GameState
initGameState = GameState 0 0 True

main :: IO ()
main = evalStateT program initGameState