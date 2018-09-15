module Application
    ( runServer
    ) where

import Web.Scotty

import Model.Calculation (calculateFromInput)
import Model.Inputs
import Model.JSON.Inputs
import Model.JSON.CalculationEntities


routes :: ScottyM ()
routes = do
  get "/" $ do
    text "Storage Costs Evaluator service"
    text "TODO: description (& form?)"
  post "/" $ do
    inputs <- jsonData :: ActionM Inputs
    let result = calculateFromInput inputs
    json result

runServer :: Int -> IO ()
runServer port = scotty port routes
