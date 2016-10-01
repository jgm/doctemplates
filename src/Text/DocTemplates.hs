{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    OverloadedStrings, GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2016 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.
This module was formerly part of pandoc and is used for pandoc's
templates.  The following program illustrates its use:

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Text
> import Data.Aeson
> import Text.DocTemplates
>
> data Employee = Employee { firstName :: String
>                          , lastName  :: String
>                          , salary    :: Maybe Int }
> instance ToJSON Employee where
>   toJSON e = object [ "name" .= object [ "first" .= firstName e
>                                        , "last"  .= lastName e ]
>                     , "salary" .= salary e ]
>
> template :: Text
> template = "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"
>
> main = case compileTemplate template of
>          Left e    -> error e
>          Right t   -> putStrLn $ renderTemplate t $ object
>                         ["employee" .=
>                           [ Employee "John" "Doe" Nothing
>                           , Employee "Omar" "Smith" (Just 30000)
>                           , Employee "Sara" "Chen" (Just 60000) ]
>                         ]

A slot for an interpolated variable is a variable name surrounded
by dollar signs.  To include a literal @$@ in your template, use
@$$@.  Variable names must begin with a letter and can contain letters,
numbers, @_@, @-@, and @.@.

The values of variables are determined by a JSON object that is
passed as a parameter to @renderTemplate@.  So, for example,
@title@ will return the value of the @title@ field, and
@employee.salary@ will return the value of the @salary@ field
of the object that is the value of the @employee@ field.

The value of a variable will be indented to the same level as the
variable.

A conditional begins with @$if(variable_name)$@ and ends with @$endif$@.
It may optionally contain an @$else$@ section.  The if section is
used if @variable_name@ has a non-null value, otherwise the else section
is used.

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

The @$for$@ keyword can be used to iterate over an array.  If
the value of the associated variable is not an array, a single
iteration will be performed on its value.

You may optionally specify separators using @$sep$@, as in the
example above.

-}

module Text.DocTemplates ( renderTemplate
                         , applyTemplate
                         , TemplateTarget(..)
                         , varListToJSON
                         , compileTemplate
                         , Template
                         ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (ToJSON(..), Value(..))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import qualified Data.Set as Set
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.List (intersperse)
import System.FilePath ((</>), (<.>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import qualified Control.Exception.Extensible as E (try, IOException)
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (preEscapedText)
import Data.ByteString.Lazy (ByteString, fromChunks)
import Data.Vector ((!?))
import Control.Applicative (many, (<|>))
import Data.Scientific (floatingOrInteger)

-- | A 'Template' is essentially a function that takes
-- a JSON 'Value' and produces 'Text'.
newtype Template = Template { unTemplate :: Value -> Text }
                 deriving Monoid

type Variable = [Text]

class TemplateTarget a where
  toTarget :: Text -> a

instance TemplateTarget Text where
  toTarget = id

instance TemplateTarget String where
  toTarget = T.unpack

instance TemplateTarget ByteString where
  toTarget = fromChunks . (:[]) . encodeUtf8

instance TemplateTarget Html where
  toTarget = preEscapedText

-- | A convenience function for passing in an association
-- list of string values instead of a JSON 'Value'.
varListToJSON :: [(String, String)] -> Value
varListToJSON assoc = toJSON $ M.fromList assoc'
  where assoc' = [(T.pack k, toVal [T.pack z | (y,z) <- assoc,
                                                not (null z),
                                                y == k])
                        | k <- ordNub $ map fst assoc ]
        toVal [x] = toJSON x
        toVal []  = Null
        toVal xs  = toJSON xs

-- An efficient specialization of nub.
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | Compile a template.
compileTemplate :: Text -> Either String Template
compileTemplate template =
  case P.parse (pTemplate <* P.eof) "template" template of
       Left e   -> Left (show e)
       Right x  -> Right x

-- | Render a compiled template using @context@ to resolve variables.
renderTemplate :: (ToJSON a, TemplateTarget b) => Template -> a -> b
renderTemplate (Template f) context = toTarget $ f $ toJSON context

-- | Combines `renderTemplate` and `compileTemplate`.
applyTemplate :: (ToJSON a, TemplateTarget b) => Text -> a -> Either String b
applyTemplate t context =
  case compileTemplate t of
         Left e   -> Left e
         Right f  -> Right $ renderTemplate f context

var :: Variable -> Template
var = Template . resolveVar

resolveVar :: Variable -> Value -> Text
resolveVar var' val =
  case multiLookup var' val of
       Just (Array vec) -> maybe mempty (resolveVar []) $ vec !? 0
       Just (String t)  -> T.stripEnd t
       Just (Number n)  -> case floatingOrInteger n of
                                   Left r  -> T.pack $ show r
                                   Right i -> T.pack $ show i
       Just (Bool True) -> "true"
       Just (Object _)  -> "true"
       Just _           -> mempty
       Nothing          -> mempty

multiLookup :: [Text] -> Value -> Maybe Value
multiLookup [] x = Just x
multiLookup (v:vs) (Object o) = H.lookup v o >>= multiLookup vs
multiLookup _ _ = Nothing

lit :: Text -> Template
lit = Template . const

cond :: Variable -> Template -> Template -> Template
cond var' (Template ifyes) (Template ifno) = Template $ \val ->
  case resolveVar var' val of
       "" -> ifno val
       _  -> ifyes val

iter :: Variable -> Template -> Template -> Template
iter var' template sep = Template $ \val -> unTemplate
  (case multiLookup var' val of
           Just (Array vec) -> mconcat $ intersperse sep
                                       $ map (setVar template var')
                                       $ toList vec
           Just x           -> cond var' (setVar template var' x) mempty
           Nothing          -> mempty) val

setVar :: Template -> Variable -> Value -> Template
setVar (Template f) var' val = Template $ f . replaceVar var' val

replaceVar :: Variable -> Value -> Value -> Value
replaceVar []     new _          = new
replaceVar (v:vs) new (Object o) =
  Object $ H.adjust (\x -> replaceVar vs new x) v o
replaceVar _ _ old = old

--- parsing

pTemplate :: Parser Template
pTemplate = do
  sp <- P.option mempty pInitialSpace
  rest <- mconcat <$> many (pConditional <|>
                            pFor <|>
                            pNewline <|>
                            pVar <|>
                            pLit <|>
                            pEscapedDollar)
  return $ sp <> rest

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 f = T.pack <$> P.many1 (P.satisfy f)

pLit :: Parser Template
pLit = lit <$> takeWhile1 (\x -> x /='$' && x /= '\n')

pNewline :: Parser Template
pNewline = do
  P.char '\n'
  sp <- P.option mempty pInitialSpace
  return $ lit "\n" <> sp

pInitialSpace :: Parser Template
pInitialSpace = do
  sps <- takeWhile1 (==' ')
  let indentVar = if T.null sps
                     then id
                     else indent (T.length sps)
  v <- P.option mempty $ indentVar <$> pVar
  return $ lit sps <> v

pEscapedDollar :: Parser Template
pEscapedDollar = lit "$" <$ P.try (P.string "$$")

pVar :: Parser Template
pVar = var <$> (P.try $ P.char '$' *> pIdent <* P.char '$')

pIdent :: Parser [Text]
pIdent = do
  first <- pIdentPart
  rest <- many (P.char '.' *> pIdentPart)
  return (first:rest)

pIdentPart :: Parser Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- T.pack <$> P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let id' = T.singleton first <> rest
  guard $ id' `notElem` reservedWords
  return id'

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep"]

skipEndline :: Parser ()
skipEndline = P.try $ P.skipMany (P.satisfy (`elem` (" \t" :: String))) >> P.char '\n' >> return ()

pConditional :: Parser Template
pConditional = do
  P.try $ P.string "$if("
  id' <- pIdent
  P.string ")$"
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- P.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- P.option mempty $ P.try $
                      do P.string "$else$"
                         when multiline $ P.option () skipEndline
                         pTemplate
  P.string "$endif$"
  when multiline $ P.option () skipEndline
  return $ cond id' ifContents elseContents

pFor :: Parser Template
pFor = do
  P.try $ P.string "$for("
  id' <- pIdent
  P.string ")$"
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  multiline <- P.option False $ skipEndline >> return True
  contents <- pTemplate
  sep <- P.option mempty $
           do P.try $ P.string "$sep$"
              when multiline $ P.option () skipEndline
              pTemplate
  P.string "$endfor$"
  when multiline $ P.option () skipEndline
  return $ iter id' contents sep

indent :: Int -> Template -> Template
indent 0   x            = x
indent ind (Template f) = Template $ \val -> indent' (f val)
  where indent' t = T.concat
                    $ intersperse ("\n" <> T.replicate ind " ") $ T.lines t
