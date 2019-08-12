{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.DocTemplates
import Data.Text (Text)
import Criterion.Main
import Criterion.Types (Config (..))
import Control.Monad.Identity
import Data.Aeson (object, (.=))

main :: IO ()
main = defaultMainWith defaultConfig{ timeLimit = 5.0 } $ cases

bigtext :: Text
bigtext = "Hello there $foo$. This is a big text."

cases :: [Benchmark]
cases =
  [
    bench "simple template" $
      nf (runIdentity . applyTemplate "bigtext" bigtext
            :: Context Text -> Either String Text)
        (valueToContext $ object [ "foo" .= (22 :: Int) ])
  ]
