{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.DocTemplates
import Test.Tasty.Golden
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.FilePath
import System.IO.Temp
import Data.Aeson
import Control.Monad.Identity
import System.FilePath.Glob
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))

main :: IO ()
main = withTempDirectory "test" "out." $ \tmpdir -> do
  testFiles <- glob "test/*.test"
  goldenTests <- mapM (getTest tmpdir) testFiles
  defaultMain $ testGroup "Tests"
    [ testGroup "Golden tests" goldenTests
    , testGroup "Unit tests" unitTests
    ]

unitTests :: [TestTree]
unitTests = [
    testCase "compile failure" $ do
      (res :: Either String Template)
        <- compileTemplate "" "$if(x$and$endif$"
      res @?= Left "\"template\" (line 1, column 6):\nunexpected \"$\"\nexpecting \".\" or \")\""
  , testCase "compile failure (keyword as variable)" $ do
      (res :: Either String Template) <- compileTemplate "" "$sep$"
      res @?= Left "\"template\" (line 1, column 5):\nunexpected \"$\"\nexpecting letter or digit or \"()\""
  ]

{- The test "golden" files are structured as follows:

{ "foo": ["bar", "baz"] }
.
A template with $foo$.
.
A template with bar, baz.

-}

diff :: FilePath -> FilePath -> [String]
diff ref new = ["diff", "-u", ref, new]

getTest :: FilePath -> FilePath -> IO TestTree
getTest tmpdir fp = do
  let actual = tmpdir </> takeFileName fp
  return $ goldenVsFileDiff fp diff fp actual $ do
    inp <- T.readFile fp
    let [json', template', _expected] = T.splitOn "\n.\n" inp
    let json = json' <> "\n"
    let template = template' <> "\n"
    let templatePath = replaceExtension fp ".txt"
    let Just (context :: Value) = decode' . BL.fromStrict . T.encodeUtf8 $ json
    res <- applyTemplate templatePath template context
    case res of
      Left e -> error e
      Right x -> T.writeFile actual $ json <> ".\n" <> template <> ".\n" <> x
