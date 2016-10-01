{-# LANGUAGE OverloadedStrings #-}

import Text.DocTemplates
import Test.Hspec
import Data.Text
import Data.Aeson

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
  describe "renderTemplate'" $ do
    it "works" $ do
      renderTemplate' template (object ["employee" .= employees]) `shouldBe`
        ("Hi, John. No salary data.\nHi, Omar. You make $30000.\nHi, Sara. You make $60000." :: Text)

