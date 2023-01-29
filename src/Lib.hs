module Lib where

import Control.Monad.State
import Text.Read (readMaybe)
import System.Process 
import GHC.IO.Exception (ExitCode)

clearConsole :: IO ExitCode
clearConsole = system "clear"

data GameState = GameState { score :: Int, turns :: Int, continue :: Bool }
data MenuInput 
    = Inc 
    | Reset 
    | Exit 
    deriving (Show, Enum, Bounded)

-- check if in range of menu items
maybeMenuInput :: Int -> Maybe MenuInput
maybeMenuInput i
    | i > fromEnum (maxBound :: MenuInput) = Nothing
    | i < fromEnum (minBound :: MenuInput) = Nothing
    | otherwise  = Just ([minBound :: MenuInput .. maxBound :: MenuInput] !! i)

handleInput :: MenuInput -> GameState -> GameState
handleInput input currentState = case input of
    Inc -> currentState { score = score currentState + 1, turns = turns currentState + 1 }
    Reset -> currentState { score = 0, turns = turns currentState }
    Exit -> currentState { continue = False}

program :: StateT GameState IO ()
program = do
    _ <- lift clearConsole
    currentState <- get
    if not (continue currentState) then return ()
    else do
        let currentScore = score currentState
        let currentTurns = turns currentState
        lift $ putStrLn ("The current score is: " ++ show currentScore)
        lift $ putStrLn ("The current turn is: " ++ show currentTurns)
        input <- lift getLine
        case (readMaybe input :: Maybe Int) >>= maybeMenuInput of
            Just menuInput -> modify (handleInput menuInput) >> program
            _  -> lift (putStrLn "Invalid input") >> program