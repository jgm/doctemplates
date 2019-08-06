{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Text.DocTemplates
   Copyright   : Copyright (C) 2009-2016 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This is the templating system used by pandoc. It was formerly be a
module in pandoc. It has been split off to make it easier to use
independently.

== Example of use

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
> main = do
>   res <- compileTemplate "mytemplate.txt" template
>   case res of
>          Left e    -> error e
>          Right t   -> T.putStrLn $ renderTemplate t $ object
>                         ["employee" .=
>                           [ Employee "John" "Doe" Nothing
>                           , Employee "Omar" "Smith" (Just 30000)
>                           , Employee "Sara" "Chen" (Just 60000) ]
>                         ]

== Delimiters

To mark variables and control structures in the template, either @$@…@$@
or @${@…@}@ may be used as delimiters. The styles may also be mixed in
the same template, but the opening and closing delimiter must match in
each case. The opening delimiter may be followed by one or more spaces
or tabs, which will be ignored. The closing delimiter may be followed by
one or more spaces or tabs, which will be ignored.

To include a literal @$@ in the document, use @$$@.

== Comments

Anything between the sequence @$--@ and the end of the line will be
treated as a comment and omitted from the output.

== Interpolated variables

A slot for an interpolated variable is a variable name surrounded by
matched delimiters. Variable names must begin with a letter and can
contain letters, numbers, @_@, @-@, and @.@. The keywords @it@, @if@,
@else@, @endif@, @for@, @sep@, and @endfor@ may not be used as variable
names. Examples:

> $foo$
> $foo.bar.baz$
> $foo_bar.baz-bim$
> $ foo $
> ${foo}
> ${foo.bar.baz}
> ${foo_bar.baz-bim}
> ${ foo }

The values of variables are determined by a JSON object that is passed
as a parameter to @renderTemplate@. So, for example, @title@ will return
the value of the @title@ field, and @employee.salary@ will return the
value of the @salary@ field of the object that is the value of the
@employee@ field.

-   If the value of the variable is a JSON string, the string will be
    rendered verbatim. (Note that no escaping is done on the string; the
    assumption is that the calling program will escape the strings
    appropriately for the output format.)
-   If the value is a JSON array, the values will be concatenated.
-   If the value is a JSON object, the string @true@ will be rendered.
-   If the value is a JSON number, it will be rendered as an integer if
    possible, otherwise as a floating-point number.
-   If the value is a JSON boolean, it will be rendered as @true@ if
    true, and as the empty string if false.
-   Every other value will be rendered as the empty string.

The value of a variable that occurs by itself on a line
will be indented to the same level as the opening delimiter of
the variable.

== Conditionals

A conditional begins with @if(variable)@ (enclosed in matched
delimiters) and ends with @endif@ (enclosed in matched delimiters). It
may optionally contain an @else@ (enclosed in matched delimiters). The
@if@ section is used if @variable@ has a non-empty value, otherwise the
@else@ section is used (if present). (Note that even the string @false@
counts as a true value.) Examples:

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
> ${if(foo)}bar${endif}
>
> ${if(foo)}
>   ${foo}
> ${endif}
>
> ${if(foo)}
> ${ foo.bar }
> ${else}
> no foo!
> ${endif}

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

== For loops

A for loop begins with @for(variable)@ (enclosed in matched delimiters)
and ends with @endfor@ (enclosed in matched delimiters. If @variable@ is
an array, the material inside the loop will be evaluated repeatedly,
with @variable@ being set to each value of the array in turn. If the
value of the associated variable is not an array, a single iteration
will be performed on its value.

Examples:

> $for(foo)$$foo$$sep$, $endfor$
>
> $for(foo)$
>   - $foo.last$, $foo.first$
> $endfor$
>
> ${ for(foo.bar) }
>   - ${ foo.bar.last }, ${ foo.bar.first }
> ${ endfor }

You may optionally specify a separator between consecutive values using
@sep@ (enclosed in matched delimiters). The material between @sep@ and
the @endfor@ is the separator.

> ${ for(foo) }${ foo }${ sep }, ${ endfor }

Instead of using @variable@ inside the loop, the special anaphoric
keyword @it@ may be used.

> ${ for(foo.bar) }
>   - ${ it.last }, ${ it.first }
> ${ endfor }

== Partials

Partials (subtemplates stored in different files) may be included using
the syntax

> ${ boilerplate() }

The partials are obtained using @getPartial@ from the @TemplateMonad@
class. This may be implemented differently in different monads. The path
passed to @getPartial@ is computed on the basis of the original template
path (a parameter to @compileTemplate@) and the partial’s name. The
partial’s name is substituted for the /base name/ of the original
template path (leaving the original template’s extension), unless the
partial has an explicit extension, in which case this is kept. So, with
the @TemplateMonad@ instance for IO, partials will be sought in the
directory containing the main template, and will be assumed to have the
extension of the main template.

Partials may optionally be applied to variables using a colon:

> ${ date:fancy() }
>
> ${ articles:bibentry() }

If @articles@ is an array, this will iterate over its values, applying
the partial @bibentry()@ to each one. So the second example above is
equivalent to

> ${ for(articles) }
> ${ it:bibentry() }
> ${ endfor }

Final newlines are omitted from included partials.

Partials may include other partials. If you exceed a nesting level of
50, though, in resolving partials, the literal @(loop)@ will be
returned, to avoid infinite loops.

A separator between values of an array may be specified in square
brackets, immediately after the variable name or partial:

> ${months[, ]}$
>
> ${articles:bibentry()[; ]$

The separator in this case is literal and (unlike with @sep@ in an
explicit @for@ loop) cannot contain interpolated variables or other
template directives.

-}

module Text.DocTemplates ( renderTemplate
                         , compileTemplate
                         , applyTemplate
                         , TemplateMonad(..)
                         , Template(..)
                         , TemplatePart(..)
                         , Variable(..)
                         , Indented(..)
                         ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (Value(..), ToJSON(..))
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import Control.Monad.Except
import Control.Exception
import System.IO.Error (isDoesNotExistError)
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String (IsString(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.List (intersperse)
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger)
import Data.Semigroup (Semigroup, (<>))
import System.FilePath

newtype Template = Template { unTemplate :: [TemplatePart] }
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Template where
  Template xs <> Template ys = Template (xs <> ys)

instance Monoid Template where
  mempty = Template []
#else
instance Monoid Template where
  mappend (Template xs) (Template ys) = Template (mappend xs ys)
  mempty = Template []
#endif

data Indented = Indented | Unindented
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

data TemplatePart =
       Interpolate Indented Variable
     | Conditional Variable Template Template
     | Iterate Variable Template Template
     | Partial Template
     | Literal Text
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

newtype Variable = Variable { unVariable :: [Text] }
  deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Variable where
  Variable xs <> Variable ys = Variable (xs <> ys)

instance Monoid Variable where
  mempty = Variable []
#else
instance Monoid Variable where
  mappend (Variable xs) (Variable ys) = Variable (mappend xs ys)
  mempty = Variable []
#endif

renderTemplate :: ToJSON a => Template -> a -> Text
renderTemplate t context = evalState (renderer t (toJSON context)) 0

it :: Variable
it = Variable ["it"]

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
       Interpolate indented v -> do
         f <- case indented of
                 Indented -> do
                   ind <- get
                   return (indent ind)
                 _        -> return id
         let t = f $ resolveVar v val
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
             iters <- mapM (\iterval -> renderer t .
                                replaceVar v iterval .
                                replaceVar it iterval $
                                val) (toList vec)
             return $ mconcat $ intersperse sep' iters
           Just val' -> renderer t $ replaceVar it val' val
           Nothing -> return mempty
       Partial t -> renderer t val

class Monad m => TemplateMonad m where
  getPartial  :: FilePath -> Parser m Text

instance TemplateMonad Identity where
  getPartial s  = fail $ "Could not get partial: " <> s

instance TemplateMonad IO where
  getPartial s  = do
    res <- liftIO $ tryJust (guard . isDoesNotExistError)
              (TIO.readFile s)
    case res of
      Left _  -> fail $ "Could not get partial " ++ s
      Right x -> return x

compileTemplate :: TemplateMonad m
                => FilePath -> Text -> m (Either String Template)
compileTemplate templPath template = do
  res <- P.runParserT (pTemplate <* P.eof)
           PState{ templatePath   = templPath
                 , partialNesting = 1
                 , beginsLine = True } "template" template
  case res of
       Left e   -> return $ Left $ show e
       Right x  -> return $ Right x

applyTemplate :: (TemplateMonad m, ToJSON a)
              => FilePath -> Text -> a -> m (Either String Text)
applyTemplate fp t val =
  fmap (`renderTemplate` val) <$> compileTemplate fp t

data PState =
  PState { templatePath   :: FilePath
         , partialNesting :: Int
         , beginsLine     :: Bool }

type Parser = P.ParsecT Text PState

pTemplate :: TemplateMonad m => Parser m Template
pTemplate = do
  ts <- many $ P.try
         (P.skipMany pComment *> (pLit <|> pDirective <|> pEscape))
  P.skipMany pComment
  return $ Template ts

pLit :: Monad m => Parser m TemplatePart
pLit = do
  cs <- mconcat <$> P.many1 (P.many1 (P.satisfy (/= '$')))
  P.updateState $ \st ->
    st{ beginsLine =
          case dropWhile (\c -> c == ' ' || c == '\t') $ reverse cs of
            ('\n':_) -> True
            []       -> beginsLine st
            _        -> False }
  return $ Literal $ fromString cs

backupSourcePos :: Monad m => Int -> Parser m ()
backupSourcePos n = do
  pos <- P.getPosition
  P.setPosition $ P.incSourceColumn pos (- n)

pEscape :: Monad m => Parser m TemplatePart
pEscape = Literal "$" <$ P.try (P.string "$$" <* backupSourcePos 1)

pDirective :: TemplateMonad m => Parser m TemplatePart
pDirective = do
  res <- pConditional <|> pForLoop <|> pInterpolate <|> pBarePartial
  col <- P.sourceColumn <$> P.getPosition
  P.updateState $ \st -> st{ beginsLine = col == 1 }
  return res

pEnclosed :: Monad m => Parser m a -> Parser m a
pEnclosed parser = P.try $ do
  closer <- pOpen
  P.skipMany pSpaceOrTab
  result <- parser
  P.skipMany pSpaceOrTab
  closer
  return result

pParens :: Monad m => Parser m a -> Parser m a
pParens parser = do
  P.char '('
  result <- parser
  P.char ')'
  return result

pConditional :: TemplateMonad m => Parser m TemplatePart
pConditional = do
  v <- pEnclosed $ P.try $ P.string "if" *> pParens pVar
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

skipEndline :: Monad m => Parser m ()
skipEndline = P.try $ P.skipMany pSpaceOrTab <* P.char '\n'

pForLoop :: TemplateMonad m => Parser m TemplatePart
pForLoop = do
  v <- pEnclosed $ P.try $ P.string "for" *> pParens pVar
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

pInterpolate :: TemplateMonad m => Parser m TemplatePart
pInterpolate = do
  begins <- beginsLine <$> P.getState
  res <- pEnclosed $ do
    var <- pVar
    (P.char ':' *> pPartial (Just var))
      <|> do separ <- pSep
             return (Iterate var (Template [Interpolate Unindented it])
                      separ)
      <|> return (Interpolate Unindented var)
  ends <- P.lookAhead $ P.option False $
             True <$ P.try (P.skipMany pSpaceOrTab *> P.newline)
  case (begins && ends, res) of
    (True, Interpolate _ v)
               -> return $ Interpolate Indented v
    (True, Iterate v (Template [Interpolate _ v']) s)
               -> return $ Iterate v (Template [Interpolate Indented v']) s
    _ -> return res

pBarePartial :: TemplateMonad m => Parser m TemplatePart
pBarePartial = pEnclosed $ pPartial Nothing

pPartial :: TemplateMonad m => Maybe Variable -> Parser m TemplatePart
pPartial mbvar = do
  fp <- P.many1 (P.alphaNum <|> P.oneOf ['_','-','.','/','\\'])
  P.string "()"
  separ <- P.option mempty pSep
  tp <- templatePath <$> P.getState
  let fp' = case takeExtension fp of
               "" -> replaceBaseName tp fp
               _  -> replaceFileName tp fp
  partial <- removeFinalNewline <$> getPartial fp'
  nesting <- partialNesting <$> P.getState
  t <- if nesting > 50
          then return $ Template [Literal "(loop)"]
          else do
            oldInput <- P.getInput
            oldPos <- P.getPosition
            P.setPosition $ P.initialPos fp
            P.setInput partial
            P.updateState $ \st -> st{ partialNesting = nesting + 1 }
            res <- pTemplate <* P.eof
            P.updateState $ \st -> st{ partialNesting = nesting }
            P.setInput oldInput
            P.setPosition oldPos
            return res
  case mbvar of
    Just var -> return $ Iterate var t separ
    Nothing  -> return $ Partial t

pSep :: Monad m => Parser m Template
pSep = do
    P.char '['
    xs <- P.many (P.satisfy (/= ']'))
    P.char ']'
    return $ Template [Literal (fromString xs)]

removeFinalNewline :: Text -> Text
removeFinalNewline t =
  case T.unsnoc t of
    Just (t', '\n') -> t'
    _               -> t

pSpaceOrTab :: Monad m => Parser m Char
pSpaceOrTab = P.satisfy (\c -> c == ' ' || c == '\t')

pComment :: Monad m => Parser m ()
pComment = do
  pos <- P.getPosition
  P.try (P.string "$--")
  P.skipMany (P.satisfy (/='\n'))
  -- If the comment begins in the first column, the line ending
  -- will be consumed; otherwise not.
  when (P.sourceColumn pos == 1) $ () <$ do
    P.char '\n'
    P.updateState $ \st -> st{ beginsLine = True }

pOpenDollar :: Monad m => Parser m (Parser m ())
pOpenDollar =
  pCloseDollar <$ P.try (P.char '$' <*
                   P.notFollowedBy (P.char '$' <|> P.char '{'))
  where
   pCloseDollar = () <$ P.char '$'

pOpenBraces :: Monad m => Parser m (Parser m ())
pOpenBraces =
  pCloseBraces <$ P.try (P.string "${" <* P.notFollowedBy (P.char '}'))
  where
   pCloseBraces = () <$ P.try (P.char '}')

pOpen :: Monad m => Parser m (Parser m ())
pOpen = pOpenDollar <|> pOpenBraces

pVar :: Monad m => Parser m Variable
pVar = do
  first <- pIdentPart <|> "it" <$ P.try (P.string "it")
  rest <- P.many $ P.char '.' *> pIdentPart
  return $ Variable (first:rest)

pIdentPart :: Monad m => Parser m Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- fromString <$>
            P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let part = T.singleton first <> rest
  guard $ part `notElem` reservedWords
  return part

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep","it"]

resolveVar :: Variable -> Value -> Text
resolveVar (Variable var') val =
  case multiLookup var' val of
       Just (Array vec) -> mconcat $ map (resolveVar mempty) $ V.toList vec
       Just (String t)  -> T.stripEnd t
       Just (Number n)  -> case floatingOrInteger n of
                                   Left (r :: Double)   -> fromString $ show r
                                   Right (i :: Integer) -> fromString $ show i
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
replaceVar (Variable (v:vs)) new (Object o) = Object $ H.alter f v o
    where f Nothing  = Just new
          f (Just x) = Just (replaceVar (Variable vs) new x)
replaceVar _ _ old = old

indent :: Int -> Text -> Text
indent 0   = id
indent ind = T.intercalate ("\n" <> T.replicate ind " ") . T.lines

