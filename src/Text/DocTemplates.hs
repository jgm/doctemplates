{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

TODO: Add haddocks generated from README.md.

-}

module Text.DocTemplates ( renderTemplate
                         , compileTemplate
                         , applyTemplate
                         , TemplateMonad(..)
                         , Template(..)
                         , TemplatePart(..)
                         , Variable(..)
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

data TemplatePart =
       Interpolate Variable
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
             iters <- mapM (\iterval -> renderer t .
                                replaceVar v iterval .
                                replaceVar (Variable ["it"]) iterval $
                                val) (toList vec)
             return $ mconcat $ intersperse sep' iters
           _ -> case resolveVar v val of
                  "" -> return mempty
                  _  -> renderer t val
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
                 , partialNesting = 1 } "template" template
  case res of
       Left e   -> return $ Left $ show e
       Right x  -> return $ Right x

applyTemplate :: (TemplateMonad m, ToJSON a)
              => FilePath -> Text -> a -> m (Either String Text)
applyTemplate fp t val =
  fmap (flip renderTemplate val) <$> compileTemplate fp t

data PState =
  PState { templatePath   :: FilePath
         , partialNesting :: Int }

type Parser = P.ParsecT Text PState

pTemplate :: TemplateMonad m => Parser m Template
pTemplate = do
  ts <- many $ P.try
         (P.skipMany pComment *> (pLit <|> pDirective <|> pEscape))
  P.skipMany pComment
  return $ Template ts

pLit :: Monad m => Parser m TemplatePart
pLit = Literal . mconcat <$>
  P.many1 (T.pack <$> P.many1 (P.satisfy (/= '$')))

backupSourcePos :: Monad m => Int -> Parser m ()
backupSourcePos n = do
  pos <- P.getPosition
  P.setPosition $ P.incSourceColumn pos (- n)

pEscape :: Monad m => Parser m TemplatePart
pEscape = (Literal "$" <$ P.try (P.string "$$" <* backupSourcePos 1))

pDirective :: TemplateMonad m => Parser m TemplatePart
pDirective = pConditional <|> pForLoop <|> pInterpolate <|> pBarePartial

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
pInterpolate = pEnclosed $ do
  var <- pVar
  (P.char ':' *> pPartial (Just var))
    <|> do separ <- pSep
           return (Iterate var (Template [Interpolate (Variable ["it"])])
                    separ)
    <|> return (Interpolate var)

pBarePartial :: TemplateMonad m => Parser m TemplatePart
pBarePartial = pEnclosed $ pPartial Nothing

pPartial :: TemplateMonad m => Maybe Variable -> Parser m TemplatePart
pPartial mbvar = do
  fp <- P.many1 (P.alphaNum <|> P.oneOf ['_','-','.'])
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
    xs <- P.many (P.satisfy (\c -> c /= ']'))
    P.char ']'
    return $ Template [Literal (T.pack xs)]

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
  when (P.sourceColumn pos == 1) $ () <$ P.char '\n'
  return ()

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
  first <- pIdentPart <|> "it" <$ (P.try (P.string "it"))
  rest <- P.many $ P.char '.' *> pIdentPart
  return $ Variable (first:rest)

pIdentPart :: Monad m => Parser m Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- T.pack <$>
            P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let part = T.singleton first <> rest
  guard $ part `notElem` reservedWords
  return part

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep","it"]

resolveVar :: Variable -> Value -> Text
resolveVar (Variable var') val =
  case multiLookup var' val of
       Just (Array vec) -> T.intercalate ", " $
                           map (resolveVar mempty) $ V.toList vec
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
replaceVar (Variable (v:vs)) new (Object o) = Object $ H.alter f v o
    where f Nothing  = Just new
          f (Just x) = Just (replaceVar (Variable vs) new x)
replaceVar _ _ old = old

indent :: Int -> Text -> Text
indent 0   = id
indent ind = T.intercalate ("\n" <> T.replicate ind " ") . T.lines

