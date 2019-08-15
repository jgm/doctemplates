{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.DocTemplates
import Data.Text (Text)
import Criterion.Main
import Criterion.Types (Config (..))
import Control.Monad.Identity
import Data.Aeson (object, (.=), Value)

main :: IO ()
main = do
  Right bigtextTemplate <- compileTemplate "bigtext.txt" bigtext
  defaultMainWith defaultConfig{ timeLimit = 5.0 } $
   [ bench "applyTemplate" $
      nf (runIdentity . applyTemplate "bigtext" bigtext
          :: Value -> Either String Text)
        val
   , bench "renderTemplate" $
      nf (renderTemplate bigtextTemplate :: Value -> Text)
        val
   ]

bigtext :: Text
bigtext = "Hello there $foo$. This is a big text.\n$for(bar)$$bar.baz$$endfor$"

val :: Value
val = object [ "foo" .= (22 :: Int)
             , "bar" .= [ object [ "baz" .= ("Hello"::Text) ]
                        , object [ "baz" .= ("Bye"::Text) ]
                        ]
             ]

