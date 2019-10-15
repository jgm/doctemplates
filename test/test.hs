{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.DocLayout (render)
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
import System.FilePath.Glob
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import Data.Maybe

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
      res <- compileTemplate "" "$if(x$and$endif$"
      res @?= Left "(line 1, column 6):\nunexpected \"$\"\nexpecting \".\", \"/\" or \")\""
  , testCase "compile failure (keyword as variable)" $ do
      res <- compileTemplate "foobar.txt" "$sep$"
      res @?= Left "\"foobar.txt\" (line 1, column 5):\nunexpected \"$\"\nexpecting letter or digit or \"()\""
  , testCase "compile failure (error in partial)" $ do
      res <- compileTemplate "test/foobar.txt" "$bad()$"
      res @?= Left "\"test/bad.txt\" (line 2, column 7):\nunexpected \"s\"\nexpecting \"$\""
  , testCase "comment with no newline" $ do
      res <- compileTemplate "foo" "$-- hi"
      res @?= Right mempty
  , testCase "reflow" $ do
      templ <- compileTemplate "foo" "not breakable and$~$ this is breakable\nok? $foo$$~$"
      let res :: T.Text
          res = case templ of
                  Right t -> render (Just 10)
                   (renderTemplate t (object ["foo" .= ("42" :: T.Text)]))
                  Left e  -> T.pack e
      res @?= "not breakable and\nthis is\nbreakable\nok? 42"
  ]

{- The test "golden" files are structured as follows:

{ "foo": ["bar", "baz"] }
.
A template with $foo$.
.
A template with bar, baz.

-}

diff :: FilePath -> FilePath -> [String]
diff ref new = ["diff", "-u", "--minimal", ref, new]

getTest :: FilePath -> FilePath -> IO TestTree
getTest tmpdir fp = do
  let actual = tmpdir </> takeFileName fp
  return $ goldenVsFileDiff fp diff fp actual $ do
    inp <- T.readFile fp
    let (j, template', _expected) =
            case T.splitOn "\n.\n" inp of
              [x,y,z] -> (x,y,z)
              _       -> error $ "Error parsing " ++ fp
    let j' = j <> "\n"
    let template = template' <> "\n"
    let templatePath = replaceExtension fp ".txt"
    let (context :: Value) = fromMaybe Null $
           decode' . BL.fromStrict . T.encodeUtf8 $ j'
    res <- applyTemplate templatePath template context
    case res of
      Left e -> error e
      Right x -> T.writeFile actual $ j' <> ".\n" <> template <> ".\n" <> x
