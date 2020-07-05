module Servant.StreamlySpec
  ( main
  , spec
  )
where

import           Control.Monad.Trans.Resource   ( ResourceT
                                                , runResourceT
                                                )
import qualified Streamly
import qualified Streamly.Prelude              as Streamly
import           Servant
import qualified Servant.API.Stream            as Servant
import qualified Servant.Types.SourceT         as Servant
import qualified Servant.Streamly
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Servant.Streamly" $ do
  let content = [0 .. 4]
  describe "Direct call" $ do
    it "toSourceIO should be equivalent to source (IO)" $ do
      let
        ts = Servant.toSourceIO
          (Streamly.fromList content :: Streamly.SerialT IO Int)
      Streamly.toList (Servant.fromSourceIO ts) `shouldReturn` content
    it "toSourceIO should be equivalent to source (ResourceT IO)" $ do
      let ts = Servant.toSourceIO
            (Streamly.fromList content :: Streamly.SerialT (ResourceT IO) Int)
      Streamly.toList (Servant.fromSourceIO ts) `shouldReturn` content
    it "fromSourceIO should be equivalent to fromList"
      $ Streamly.toList (Servant.fromSourceIO (Servant.source content))
      `shouldReturn` content
