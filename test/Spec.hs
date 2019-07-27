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

data Worksite = Worksite { location  :: String
                         , workers   :: [Employee] }
instance ToJSON Worksite where
  toJSON e = object [ "location" .= location e
                    , "workers"  .= workers e ]

worksite :: Worksite
worksite = Worksite "canyon" employees

template :: Text
template =
  "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $$$employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"

template2 :: Text
template2 =
  "$--\n${ for(employee) }Hi, ${ employee.name.first }. ${ if(employee.salary) }You make $$${ employee.salary }.${ else }No salary data.${ endif }${ sep }\n${ endfor }"

template3 :: Text
template3 =
  "${ for(employee) }Hi, ${ it.name.first }. ${ if(it.salary) }You make $$${ it.salary }.${ else }No salary data.${ endif }${ sep }\n${ endfor }"

template4 :: Text
template4 =
  "${ for(worksite.workers) }\n${it.name.last}, ${it.name.first}\n${ endfor }"

main :: IO ()
main = hspec $ do
  describe "applyTemplate" $ do
    it "works" $ do
      applyTemplate template (object ["employee" .= employees])
        `shouldBe`
        (Right "Hi, John. No salary data.\nHi, Omar. You make $30000.\nHi, Sara. You make $60000." :: Either String Text)

    it "works with ${} delimiters" $ do
      applyTemplate template2 (object ["employee" .= employees])
        `shouldBe`
        (Right "Hi, John. No salary data.\nHi, Omar. You make $30000.\nHi, Sara. You make $60000." :: Either String Text)

    it "works with variables starting with it." $ do
      applyTemplate template3 (object ["employee" .= employees])
        `shouldBe`
        (Right "Hi, John. No salary data.\nHi, Omar. You make $30000.\nHi, Sara. You make $60000." :: Either String Text)

    it "handles for loops within object fields" $ do
      applyTemplate template4 (object ["worksite" .= worksite])
        `shouldBe`
        (Right "Doe, John\nSmith, Omar\nChen, Sara\n" :: Either String Text)

    it "renders numbers appropriately as integer or floating" $ do
      applyTemplate "$m$ and $n$"
        (object ["m" .= (5 :: Integer), "n" .= (7.3 :: Double)])
        `shouldBe`
        (Right "5 and 7.3" :: Either String Text)

    it "handles comments" $ do
      applyTemplate "hello $--there and $m$\n$-- comment\nbar"
        (object ["m" .= (5 :: Integer)])
        `shouldBe`
        (Right "hello \nbar" :: Either String Text)

    it "fails with an incorrect template" $ do
      applyTemplate "$if(x$and$endif$" (object [])
        `shouldBe`
        (Left "\"template\" (line 1, column 6):\nunexpected \"$\"\nexpecting \".\" or \")\"" :: Either String Text)

