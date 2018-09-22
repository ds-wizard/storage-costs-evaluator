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
  get "/" $
    file "./static/index.html"
  get "/script.js" $
    file "./static/script.js"
  post "/" $ do
    inputs <- jsonData :: ActionM Inputs
    let result = calculateFromInput inputs
    json result

runServer :: Int -> IO ()
runServer port = scotty port routes
