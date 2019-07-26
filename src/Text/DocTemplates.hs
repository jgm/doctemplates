{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

To mark variables and control structures in the template, either @$@…@$@
or @{{@…@}}@ may be used as delimiters. The styles may also be mixed in
the same template, but the opening and closing delimiter must match in
each case. The opening delimiter may be followed by one or more spaces
or tabs, which will be ignored. The closing delimiter may be followed by
one or more spaces or tabs, which will be ignored.

To include a literal @$@ in the document, use @$$@. To include a literal
@{{@, use @{{{{@.

Anything between the sequence @$--@ or @{{@@--@ and the end of the line
will be treated as a comment and omitted from the output.

A slot for an interpolated variable is a variable name surrounded by
matched delimiters. Variable names must begin with a letter and can
contain letters, numbers, @_@, @-@, and @.@. The keywords @if@, @else@,
@endif@, @for@, @sep@, and @endfor@ may not be used as variable names.
Examples:

> $foo$
> $foo.bar.baz$
> $foo_bar.baz-bim$
> $ foo $
> {{foo}}
> {{foo.bar.baz}}
> {{foo_bar.baz-bim}}
> {{ foo }}

The values of variables are determined by a JSON object that is passed
as a parameter to @renderTemplate@. So, for example, @title@ will return
the value of the @title@ field, and @employee.salary@ will return the
value of the @salary@ field of the object that is the value of the
@employee@ field.

The value of a variable will be indented to the same level as the
opening delimiter of the variable.

A conditional begins with @if(variable)@ (enclosed in matched
delimiters) and ends with @endif@ (enclosed in matched delimiters). It
may optionally contain an @else@ (enclosed in matched delimiters). The
@if@ section is used if @variable@ has a non-null value, otherwise the
@else@ section is used (if present). Examples:

> $if(foo)$bar$endif$
>
> $if(foo)$
>   $foo$
> $endif$
>
> $if(foo)$
> part one
> $else$
> part two
> $endif$
>
> {{if(foo)}}bar{{endif}}
>
> {{if(foo)}}
>   {{foo}}
> {{endif}}
>
> {{if(foo)}}
> {{ foo.bar }}
> {{else}}
> no foo!
> {{endif}}

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

A for loop begins with @for(variable)@ (enclosed in matched delimiters)
and ends with @endfor@ (enclosed in matched delimiters. If @variable@ is
an array, the material inside the loop will be evaluated repeatedly,
with @variable@ being set to each value of the array in turn. If the
value of the associated variable is not an array, a single iteration
will be performed on its value.

You may optionally specify a separator between consecutive values using
@sep@ (enclosed in matched delimiters). The material between @sep@ and
the @endfor@ is the separator.

Examples:

> $for(foo)$$foo$$sep$, $endfor$
>
> $for(foo)$
>   - $foo.last$, $foo.first$
> $endfor$
>
> {{ for(foo) }}{{ foo }}{{ sep }}, {{ endfor }}
>
> {{ for(foo) }}
>   - {{ foo.last }}, {{ foo.first }}
> {{ endfor }}

-}

module Text.DocTemplates ( renderTemplate
                         , compileTemplate
                         , applyTemplate
                         , Template(..)
                         , TemplatePart(..)
                         , Variable(..)
                         ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (Value(..), ToJSON(..))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.List (intersperse)
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import Data.Vector ((!?))
import Data.Scientific (floatingOrInteger)
import Data.Semigroup (Semigroup)

newtype Template = Template { unTemplate :: [TemplatePart] }
     deriving (Show, Read, Data, Typeable, Generic)

instance Semigroup Template where
  Template xs <> Template ys = Template (xs <> ys)

instance Monoid Template where
  mempty = Template []

data TemplatePart =
       Interpolate Variable
     | Conditional Variable Template Template
     | Iterate Variable Template Template
     | Literal Text
     deriving (Show, Read, Data, Typeable, Generic)

newtype Variable = Variable { unVariable :: [Text] }
  deriving (Show, Read, Data, Typeable, Generic)

instance Semigroup Variable where
  Variable xs <> Variable ys = Variable (xs <> ys)

instance Monoid Variable where
  mempty = Variable []

renderTemplate :: ToJSON a => Template -> a -> Text
renderTemplate t context = evalState (renderer t (toJSON context)) 0

renderer :: Template -> Value -> State Int Text
renderer (Template xs) val = mconcat <$> mapM renderPart xs
  where
   modifyIndent t = do
     ind <- get
     put $ T.foldl' (\cur c ->
                 case c of
                   '\n' -> 0
                   _    -> cur + 1) ind t
   renderPart x =
     case x of
       Literal t -> do
         modifyIndent t
         return t
       Interpolate v -> do
         ind <- get
         let t = indent ind $ resolveVar v val
         modifyIndent t
         return t
       Conditional v ift elset -> renderer branch val
         where branch = case resolveVar v val of
                          "" -> elset
                          _  -> ift
       Iterate v t sep ->
         case multiLookup (unVariable v) val of
           Just (Array vec) -> do
             sep' <- renderer sep val
             iters <- mapM (renderer t)
                         (map (\newv -> replaceVar v newv val) (toList vec))
             return $ mconcat $ intersperse sep' iters
           _ -> case resolveVar v val of
                  "" -> return mempty
                  _  -> renderer t val

compileTemplate :: Text -> Either String Template
compileTemplate template =
  case P.parse (pTemplate <* P.eof) "template" template of
       Left e   -> Left (show e)
       Right x  -> Right x

applyTemplate :: ToJSON a => Text -> a -> Either String Text
applyTemplate t val =
  case compileTemplate t of
    Left e   -> Left e
    Right ct -> Right $ renderTemplate ct val


pTemplate :: Parser Template
pTemplate = do
  ts <- many (P.skipMany pComment *> (pLit <|> pDirective <|> pEscape))
  P.skipMany pComment
  return $ Template ts

pLit :: Parser TemplatePart
pLit = Literal . mconcat <$>
  P.many1 (
     (T.pack <$> P.many1 (P.satisfy (\c -> c /= '$' && c /= '{')))
     <|>
     ("{" <$ P.try (P.char '{' >> P.notFollowedBy (P.char '{')))
     )

backupSourcePos :: Int -> Parser ()
backupSourcePos n = do
  pos <- P.getPosition
  P.setPosition $ P.incSourceColumn pos (- n)

pEscape :: Parser TemplatePart
pEscape = do
  (Literal "$" <$ P.try (P.string "$$" <* backupSourcePos 1))
  <|>
  (Literal "{{" <$ P.try (P.string "{{{{" <* backupSourcePos 2))

pDirective :: Parser TemplatePart
pDirective = pConditional <|> pForLoop <|> pInterpolate

pEnclosed :: Parser a -> Parser a
pEnclosed parser = P.try $ do
  closer <- pOpen
  P.skipMany pSpaceOrTab
  result <- parser
  P.skipMany pSpaceOrTab
  closer
  return result

pParens :: Parser a -> Parser a
pParens parser = do
  P.char '('
  result <- parser
  P.char ')'
  return result

pConditional :: Parser TemplatePart
pConditional = do
  v <- pEnclosed $ P.try (P.string "if") *> pParens pVar
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- P.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- P.option mempty $
                    do pEnclosed (P.string "else")
                       when multiline $ P.option () skipEndline
                       pTemplate
  pEnclosed (P.string "endif")
  when multiline $ P.option () skipEndline
  return $ Conditional v ifContents elseContents

skipEndline :: Parser ()
skipEndline = P.try $ P.skipMany pSpaceOrTab <* P.char '\n'

pForLoop :: Parser TemplatePart
pForLoop = do
  v <- pEnclosed $ P.try (P.string "for") *> pParens pVar
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  multiline <- P.option False $ skipEndline >> return True
  contents <- pTemplate
  sep <- P.option mempty $
           do pEnclosed (P.string "sep")
              when multiline $ P.option () skipEndline
              pTemplate
  pEnclosed (P.string "endfor")
  when multiline $ P.option () skipEndline
  return $ Iterate v contents sep

pInterpolate :: Parser TemplatePart
pInterpolate = Interpolate <$> pEnclosed pVar

pSpaceOrTab :: Parser Char
pSpaceOrTab = P.satisfy (\c -> c == ' ' || c == '\t')

pComment :: Parser ()
pComment = do
  pos <- P.getPosition
  P.try (pOpen >> P.string "--")
  P.skipMany (P.satisfy (/='\n'))
  -- If the comment begins in the first column, the line ending
  -- will be consumed; otherwise not.
  when (P.sourceColumn pos == 1) $ () <$ P.char '\n'
  return ()

pOpenDollar :: Parser (Parser ())
pOpenDollar =
  pCloseDollar <$ P.try (P.char '$' <* P.notFollowedBy (P.char '$'))
  where
   pCloseDollar = () <$ P.char '$'

pOpenBraces :: Parser (Parser ())
pOpenBraces =
  pCloseBraces <$ P.try (P.string "{{" <* P.notFollowedBy (P.string "{{"))
  where
   pCloseBraces = () <$ P.try (P.string "}}")

pOpen :: Parser (Parser ())
pOpen = pOpenDollar <|> pOpenBraces

pVar :: Parser Variable
pVar = do
  first <- pIdentPart
  rest <- many (P.char '.' *> pIdentPart)
  return $ Variable (first:rest)

pIdentPart :: Parser Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- T.pack <$>
            P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let part = T.singleton first <> rest
  guard $ part `notElem` reservedWords
  return part

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep"]

resolveVar :: Variable -> Value -> Text
resolveVar (Variable var') val =
  case multiLookup var' val of
       Just (Array vec) -> maybe mempty (resolveVar mempty) $ vec !? 0
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

replaceVar :: Variable -- ^ Field
           -> Value -- ^ New value
           -> Value -- ^ Old object
           -> Value -- ^ New object
replaceVar (Variable [])     new _          = new
replaceVar (Variable (v:vs)) new (Object o) =
  Object $ H.adjust (replaceVar (Variable vs) new) v o
replaceVar _ _ old = old

indent :: Int -> Text -> Text
indent 0   = id
indent ind = T.intercalate ("\n" <> T.replicate ind " ") . T.lines

