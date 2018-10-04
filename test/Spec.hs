import Test.Hspec

import qualified Specs.ModelSpec as ModelSpec
import qualified Specs.ServiceSpec as ServiceSpec

main :: IO ()
main =
  hspec $ do
    describe "Model computations" ModelSpec.spec
    describe "Service API" ServiceSpec.spec
