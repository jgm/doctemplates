{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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
> import Data.Text (Text)
> import qualified Data.Text.IO as T
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
> main :: IO ()
> main = case compileTemplate template of
>          Left e    -> error e
>          Right t   -> T.putStrLn $ renderTemplate t $ object
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
                         , compileTemplate
                         , Template
                         ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (ToJSON(..), Value(..))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Data.Monoid
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (intersperse)
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import Data.Vector ((!?))
import Data.Scientific (floatingOrInteger)
import Data.Semigroup (Semigroup)

-- | A 'Template' is essentially a function that takes
-- a JSON 'Value' and produces 'Text'.
newtype Template = Template { unTemplate :: Value -> Text }
                 deriving (Semigroup, Monoid)

type Variable = [Text]

-- | Compile a template.
compileTemplate :: Text -> Either String Template
compileTemplate template =
  case P.parse (pTemplate <* P.eof) "template" template of
       Left e   -> Left (show e)
       Right x  -> Right x

-- | Render a compiled template using @context@ to resolve variables.
renderTemplate :: ToJSON a => Template -> a -> Text
renderTemplate (Template f) context = f $ toJSON context

-- | Combines `renderTemplate` and `compileTemplate`.
applyTemplate :: ToJSON a => Text -> a -> Either String Text
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
                                   Left (r :: Double)   -> T.pack $ show r
                                   Right (i :: Integer) -> T.pack $ show i
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
  Object $ H.adjust (replaceVar vs new) v o
replaceVar _ _ old = old

--- parsing

pOpen :: Parser ()
pOpen = () <$ (P.char '$' >> P.skipMany pSpaceOrTab)

pClose :: Parser ()
pClose = () <$ (P.char '$' >> P.skipMany pSpaceOrTab)

pEnclosed :: Parser a -> Parser a
pEnclosed parser =
  P.try $ pOpen *> parser <* pClose

pTemplate :: Parser Template
pTemplate = do
  sp <- P.option mempty pInitialSpace
  rest <- mconcat <$> many (pConditional <|>
                            pFor <|>
                            pNewline <|>
                            pVar <|>
                            pComment <|>
                            pLit <|>
                            pEscapedDollar)
  return $ sp <> rest

pLit :: Parser Template
pLit = lit . T.pack <$>
  P.many1 (P.notFollowedBy (pOpen <|> pClose) >> P.satisfy (/='\n'))

pNewline :: Parser Template
pNewline = do
  P.char '\n'
  sp <- P.option mempty pInitialSpace
  return $ lit "\n" <> sp

pInitialSpace :: Parser Template
pInitialSpace = do
  sps <- T.pack <$> P.many1 (P.satisfy (==' '))
  let indentVar = if T.null sps
                     then id
                     else indent (T.length sps)
  v <- P.option mempty $ indentVar <$> pVar
  return $ lit sps <> v

pEscapedDollar :: Parser Template
pEscapedDollar = lit "$" <$ P.try (P.string "$$")

pComment :: Parser Template
pComment = do
  pos <- P.getPosition
  P.try (pOpen >> P.string "--")
  P.skipMany (P.satisfy (/='\n'))
  -- If the comment begins in the first column, the line ending
  -- will be consumed; otherwise not.
  when (P.sourceColumn pos == 1) $ () <$ P.char '\n'
  return mempty

pVar :: Parser Template
pVar = var <$> P.try (P.char '$' *> pIdent <* P.char '$')

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

pSpaceOrTab :: Parser Char
pSpaceOrTab = P.satisfy (\c -> c == ' ' || c == '\t')

skipEndline :: Parser ()
skipEndline = P.try $ P.skipMany pSpaceOrTab >> P.char '\n' >> return ()

pConditional :: Parser Template
pConditional = do
  id' <- pEnclosed $ P.string "if(" *> pIdent <* P.string ")"
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- P.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- P.option mempty $ P.try $
                      do pEnclosed (P.string "else")
                         when multiline $ P.option () skipEndline
                         pTemplate
  pEnclosed (P.string "endif")
  when multiline $ P.option () skipEndline
  return $ cond id' ifContents elseContents

pFor :: Parser Template
pFor = do
  id' <- pEnclosed $ P.string "for(" *> pIdent <* P.string ")"
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  multiline <- P.option False $ skipEndline >> return True
  contents <- pTemplate
  sep <- P.option mempty $
           do pEnclosed (P.string "sep")
              when multiline $ P.option () skipEndline
              pTemplate
  pEnclosed (P.string "endfor")
  when multiline $ P.option () skipEndline
  return $ iter id' contents sep

indent :: Int -> Template -> Template
indent 0   x            = x
indent ind (Template f) = Template $ \val -> indent' (f val)
  where indent' t = T.concat
                    $ intersperse ("\n" <> T.replicate ind " ") $ T.lines t
