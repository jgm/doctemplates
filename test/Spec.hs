{-# LANGUAGE OverloadedStrings #-}

import Text.DocTemplates
import Test.Hspec
import Data.Text
import Data.Aeson
import Data.Either

data Employee = Employee { firstName :: String
                         , lastName  :: String
                         , salary    :: Maybe Integer }
instance ToJSON Employee where
  toJSON e = object [ "name" .= object [ "first" .= firstName e
                                       , "last"  .= lastName e ]
                    , "salary" .= salary e ]

employees :: [Employee]
employees = [ Employee "John" "Doe" Nothing
            , Employee "Omar" "Smith" (Just 30000)
            , Employee "Sara" "Chen" (Just 60000) ]

template :: Text
template =
  "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $$$employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"

main :: IO ()
main = hspec $ do
  describe "applyTemplate" $ do
    it "works" $ do
      applyTemplate template (object ["employee" .= employees])
        `shouldBe`
        (Right "Hi, John. No salary data.\nHi, Omar. You make $30000.\nHi, Sara. You make $60000." :: Either String Text)
    it "renders numbers appropriately as integer or floating" $ do
      applyTemplate "$m$ and $n$"
        (object ["m" .= (5 :: Integer), "n" .= (7.3 :: Double)])
        `shouldBe`
        (Right "5 and 7.3" :: Either String Text)
    it "fails with an incorrect template" $ do
      applyTemplate "$if(x$and$endif$" (object [])
        `shouldBe`
        (Left "\"template\" (line 1, column 6):\nunexpected \"$\"\nexpecting \".\" or \")$\"" :: Either String Text)

