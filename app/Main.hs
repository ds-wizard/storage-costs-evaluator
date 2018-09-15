module Main where

import Data.Maybe
import System.Environment
import Text.Read

import Application (runServer)


defaultPort :: Int
defaultPort = 3000


main :: IO ()
main = do
  putStrLn "Starting Storage Costs Evaluator service"
  args <- getArgs
  let port = if length args < 1
             then defaultPort
             else fromMaybe defaultPort $ readMaybe (head args)
  runServer port
