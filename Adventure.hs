module Main where

import World
import Actions
import Parsing
import Test

import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT, InputT)
import Control.Monad ()
import Control.Monad.IO.Class (liftIO)
import System.IO ()
import System.Exit ()


{- Messages for different game outcomes -}
winmessage = "Congratulations, you have made it out of the house.\nNow go to your lectures..."

brushTeethMessage = "Congratulations, you have made it out of the house.\nBut you forgot to brush your teeth! Now you have to go to lectures with stinky breath...."

losemessage = "NOO! you forgot your house keys and have been LOCKED OUTTTT!!\nWell, that's the day ruined..."

openingMessage = "\nYou have woken up. Complete the following tasks to win the game: \n" ++
                 "- Find your clothes around the house and get dressed\n" ++
                 "- Drink some coffee\n" ++
                 "- Brush your teeth\n" ++
                 "- Collect your keys and laptop\n" ++
                 "- Leave the house for your lectures\n"

darkMessage = "You hit your head, why would you try and wander around in the dark?"

{- Given a game state and user input, output message and return a new game state for the user -}
process :: GameData -> [String] -> IO GameData
process state [cmd,arg] = case actions cmd of                                                         -- determine if user inputted an action
                              Just fn -> case objectOptions arg of
                                    Just obj -> handleGameUpdate (fn obj state)
                                    Nothing -> handleGameUpdate (state, "I don't understand")
                              Nothing -> case moves cmd of                                            -- determine if user inputted a move
                                    Just fn -> case directions arg of
                                          Just dir -> handleGameUpdate (fn dir state)
                                          Nothing -> handleGameUpdate (state, "I don't understand")
                                    Nothing -> handleGameUpdate (state, "I don't understand")
process state [cmd]     = case commands cmd of                                                        -- determine if user inputted a command
                            Just fn -> handleGameUpdate (fn state)
                            Nothing -> case ioCommands cmd of                                         -- determine if user inputted an io command
                              Just fn -> do
                                    (state',str) <- fn state
                                    putStrLn str
                                    return state'
                              Nothing -> handleGameUpdate (state, "I don't understand")
process state _ = do
      putStrLn "I don't understand"
      return state

{- Function to output action string and return new io game state-}

handleGameUpdate :: (GameData,String) -> IO GameData
handleGameUpdate (state, str) = do
      putStrLn str
      return state


{- Read-Eval-Print Loop for the game -}

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
      outputStrLn (show state)
      maybeCmd <- getInputLine "What now? "
      case maybeCmd of
            Nothing -> repl state
            Just line -> do
                  state' <- liftIO (process state (tokeniseWords line))

                  if won state' && not (gotKeys state') then do
                        outputStrLn losemessage
                        return state'
                  else if won state' && not (brushed state') then do
                        outputStrLn brushTeethMessage
                        return state'
                  else if won state' then do                                     -- Check various game outcomes and display appropriate messages
                        outputStrLn winmessage
                        return state'
                  else if gameDark state' then do
                        outputStrLn darkMessage
                        return state'
                  else repl state'

{- Main function to start the game -}

main :: IO ()
main = do
      putStrLn openingMessage
      runInputT defaultSettings (repl initState) >> return ()

wordParser :: Parser [String]
wordParser = many (token ident)

{- Tokenize a string into a list of words -}

tokeniseWords :: String -> [String]
tokeniseWords input = case parse wordParser input of
  [(words, _)] -> words
  _            -> []

runTests = Test.run
