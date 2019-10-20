{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.DocTemplates
import qualified Data.Text as T
import Data.Text (Text)
import Criterion.Main
import Criterion.Types (Config (..))
import Control.Monad.Identity
import Data.Semigroup ((<>))
import Data.Aeson (object, (.=), Value)
import Text.DocLayout (render)

main :: IO ()
main = do
  Right bigtextTemplate <- compileTemplate "bigtext.txt" bigtext
  defaultMainWith defaultConfig{ timeLimit = 5.0 } $
   [ bench "applyTemplate" $
      nf (fmap (render Nothing)
          . runIdentity . applyTemplate "bigtext" bigtext
          :: Value -> Either String Text)
        val
   , bench "renderTemplate" $
      nf (render Nothing . renderTemplate bigtextTemplate :: Value -> Text)
        val
   ]

bigtext :: Text
bigtext = T.replicate 150 $
  "Hello there $foo$. This is a big text.\n$for(bar)$$bar.baz$$endfor$\n"
  <> "$if(foo)$Hi $foo$.$endif$\n"

val :: Value
val = object [ "foo" .= (22 :: Int)
             , "bar" .= [ object [ "baz" .= ("Hello"::Text) ]
                        , object [ "baz" .= ("Bye"::Text) ]
                        ]
             ]

