module Application
    ( runServer
    , app
    ) where

import Web.Scotty
import Network.Wai (Application)
import Network.Wai.Middleware.Static

import Model.Calculation (calculateFromInput)
import Model.Inputs
import Model.JSON.Inputs
import Model.JSON.CalculationEntities



routes :: ScottyM ()
routes = do
  middleware $ staticPolicy (noDots >-> hasPrefix "static" >-> addBase "./dist")
  get "/" $
    file "./dist/index.html"
  post "/" $ do
    inputs <- jsonData :: ActionM Inputs
    let result = calculateFromInput inputs
    json result


app :: IO Application
app = scottyApp routes


runServer :: Int -> IO ()
runServer port = scotty port routes
