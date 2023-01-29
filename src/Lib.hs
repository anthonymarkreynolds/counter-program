module Lib where

import Control.Monad.State
import Text.Read (readMaybe)
import System.Process 
import GHC.IO.Exception (ExitCode)

clearConsole :: IO ExitCode
clearConsole = system "clear"

data GameState = GameState 
    { score :: Int
    , turns :: Int
    , continue :: Bool }

data MenuInput 
    = Increment 
    | Reset 
    | Exit 
    deriving (Show, Enum, Bounded)

menuInputs :: [MenuInput]
menuInputs = [minBound :: MenuInput .. maxBound :: MenuInput]

-- check if in range of menu items
maybeMenuInput :: Int -> Maybe MenuInput
maybeMenuInput i
    | n > fromEnum (maxBound :: MenuInput) = Nothing
    | n < fromEnum (minBound :: MenuInput) = Nothing
    | otherwise  = Just (menuInputs !! n)
        where n = i - 1

startTurn :: GameState -> GameState
startTurn currentState = currentState { turns = turns currentState + 1}

handleMenu :: Maybe MenuInput -> GameState -> GameState
handleMenu input currentState = startTurn $ case input of
    Just Increment -> currentState { score = score currentState + 1}
    Just Reset     -> currentState { score = 0}
    Just Exit      -> currentState { continue = False}
    Nothing        -> currentState 

printMenuInput :: MenuInput -> IO ()
printMenuInput input = putStrLn $ (show $ (fromEnum input)+1) ++ ". " ++ (show input)

renderView :: GameState -> IO ()
renderView currentState = do
    _ <- clearConsole -- Clear the screen
    putStrLn "Welcome to the counter program!"
    let currentScore = score currentState
    let currentTurn = turns currentState
    mapM_ printMenuInput menuInputs
    putStrLn ("The current score is: " ++ show currentScore)
    putStrLn ("The current turn is: " ++ show currentTurn)

program :: StateT GameState IO ()
program = do
    currentState <- get
    if not (continue currentState) then do
        lift $ putStrLn "Goodbye"
        return ()
    else do
        lift $ renderView currentState
        input <- lift getLine
        menuInput <- lift $ pure $ (readMaybe input :: Maybe Int) >>= maybeMenuInput
        modify (handleMenu menuInput) >> program