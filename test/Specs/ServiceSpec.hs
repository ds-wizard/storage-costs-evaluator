module Specs.ServiceSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai


import Application (app)


spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
