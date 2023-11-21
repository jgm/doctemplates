{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.DocTemplates.Parser
   Copyright   : Copyright (C) 2009-2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}

module Text.DocTemplates.Parser
    ( compileTemplate ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Control.Monad.Trans (lift)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import Control.Applicative
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import System.FilePath
import Text.DocTemplates.Internal
import qualified Text.DocLayout as DL
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup ((<>), Semigroup)
#endif

-- | Compile a template.  The FilePath parameter is used
-- to determine a default path and extension for partials
-- and may be left empty if partials are not used.
compileTemplate :: (TemplateMonad m, TemplateTarget a)
                => FilePath -> Text -> m (Either String (Template a))
compileTemplate templPath template = do
  res <- P.runParserT (pTemplate <* P.eof)
           PState{ templatePath    = templPath
                 , partialNesting  = 1
                 , breakingSpaces  = False
                 , firstNonspace   = P.initialPos templPath
                 , nestedCol       = Nothing
                 , insideDirective = False
                 } templPath template
  case res of
       Left e   -> return $ Left $ show e
       Right x  -> return $ Right x


data PState =
  PState { templatePath    :: FilePath
         , partialNesting  :: !Int
         , breakingSpaces  :: !Bool
         , firstNonspace   :: P.SourcePos
         , nestedCol       :: Maybe Int
         , insideDirective :: Bool
         }

type Parser = P.ParsecT Text PState

pTemplate :: (TemplateMonad m, TemplateTarget a) => Parser m (Template a)
pTemplate = do
  P.skipMany pComment
  mconcat <$> many
    ((pLit <|> pNewline <|> pDirective <|>
      pEscape) <* P.skipMany pComment)

pEndline :: Monad m => Parser m String
pEndline = P.try $ do
  nls <- pLineEnding
  mbNested <- nestedCol <$> P.getState
  inside <- insideDirective <$> P.getState
  case mbNested of
    Just col -> do
      P.skipMany $ do
        P.getPosition >>= guard . (< col) . P.sourceColumn
        P.char ' ' <|> P.char '\t'
      curcol <- P.sourceColumn <$> P.getPosition
      guard $ inside || curcol >= col
    Nothing  ->  return ()
  return nls

pBlankLine :: (TemplateTarget a, Monad m) => Parser m (Template a)
pBlankLine =
  P.try $ Literal . fromString <$> pLineEnding <* P.lookAhead pNewlineOrEof

pNewline :: (TemplateTarget a, Monad m) => Parser m (Template a)
pNewline = P.try $ do
  nls <- pEndline
  sps <- P.many (P.char ' ' <|> P.char '\t')
  breakspaces <- breakingSpaces <$> P.getState
  pos <- P.getPosition
  P.updateState $ \st -> st{ firstNonspace = pos }
  return $ Literal $
    if breakspaces
       then DL.BreakingSpace
       else fromString $ nls <> sps

pLit :: (TemplateTarget a, Monad m) => Parser m (Template a)
pLit = do
  cs <- P.many1 (P.satisfy (\c -> c /= '$' && c /= '\n' && c /= '\r'))
  when (all (\c -> c == ' ' || c == '\t') cs) $ do
     pos <- P.getPosition
     when (P.sourceLine pos == 1) $
       P.updateState $ \st -> st{ firstNonspace = pos }
  breakspaces <- breakingSpaces <$> P.getState
  if breakspaces
     then return $ toBreakable cs
     else return $ Literal $ fromString cs

toBreakable :: TemplateTarget a => String -> Template a
toBreakable [] = Empty
toBreakable xs =
  case break isSpacy xs of
    ([], []) -> Empty
    ([], zs) -> Literal DL.BreakingSpace <>
                   toBreakable (dropWhile isSpacy zs)
    (ys, []) -> Literal (fromString ys)
    (ys, zs) -> Literal (fromString ys) <> toBreakable zs

isSpacy :: Char -> Bool
isSpacy ' '  = True
isSpacy '\n' = True
isSpacy '\r' = True
isSpacy '\t' = True
isSpacy _    = False

backupSourcePos :: Monad m => Int -> Parser m ()
backupSourcePos n = do
  pos <- P.getPosition
  P.setPosition $ P.incSourceColumn pos (- n)

pEscape :: (TemplateTarget a, Monad m) => Parser m (Template a)
pEscape = Literal "$" <$ P.try (P.string "$$" <* backupSourcePos 1)

pDirective :: (TemplateTarget a, TemplateMonad m)
           => Parser m (Template a)
pDirective =
  pConditional <|> pForLoop <|> pReflowToggle <|> pNested <|>
  pInterpolate <|> pBarePartial

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

pInside :: Monad m
        => Parser m (Template a)
        -> Parser m (Template a)
pInside parser = do
  oldInside <- insideDirective <$> P.getState
  P.updateState $ \st -> st{ insideDirective = True }
  res <- parser
  P.updateState $ \st -> st{ insideDirective = oldInside }
  return res

pConditional :: (TemplateTarget a, TemplateMonad m)
             => Parser m (Template a)
pConditional = do
  v <- pEnclosed $ P.try $ P.string "if" *> pParens pVar
  pInside $ do
    multiline <- P.option False (True <$ skipEndline)
    -- if newline after the "if", then a newline after "endif" will be swallowed
    ifContents <- pTemplate
    elseContents <- P.option mempty (pElse multiline <|> pElseIf)
    pEnclosed (P.string "endif")
    when multiline $ P.option () skipEndline
    return $ Conditional v ifContents elseContents

pElse :: (TemplateTarget a, TemplateMonad m)
      => Bool -> Parser m (Template a)
pElse multiline = do
  pEnclosed (P.string "else")
  when multiline $ P.option () skipEndline
  pTemplate

pElseIf :: (TemplateTarget a, TemplateMonad m) => Parser m (Template a)
pElseIf = do
  v <- pEnclosed $ P.try $ P.string "elseif" *> pParens pVar
  multiline <- P.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- P.option mempty (pElse multiline <|> pElseIf)
  return $ Conditional v ifContents elseContents

skipEndline :: Monad m => Parser m ()
skipEndline = do
  pEndline
  pos <- P.lookAhead $ do
           P.skipMany (P.char ' ' <|> P.char '\t')
           P.getPosition
  P.updateState $ \st -> st{ firstNonspace = pos }

pReflowToggle :: (Monoid a, Semigroup a, TemplateMonad m)
              => Parser m (Template a)
pReflowToggle = do
  pEnclosed $ P.char '~'
  P.modifyState $ \st -> st{ breakingSpaces = not (breakingSpaces st) }
  return mempty

pNested :: (TemplateTarget a, TemplateMonad m) => Parser m (Template a)
pNested = do
  col <- P.sourceColumn <$> P.getPosition
  pEnclosed $ P.char '^'
  oldNested <- nestedCol <$> P.getState
  P.updateState $ \st -> st{ nestedCol = Just col }
  x <- pTemplate
  xs <- P.many $ P.try $ do
          y <- mconcat <$> P.many1 pBlankLine
          z <- pTemplate
          return (y <> z)
  let contents = x <> mconcat xs
  P.updateState $ \st -> st{ nestedCol = oldNested }
  return $ Nested contents

pForLoop :: (TemplateTarget a, TemplateMonad m) => Parser m (Template a)
pForLoop = do
  v <- pEnclosed $ P.try $ P.string "for" *> pParens pVar
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  pInside $ do
    multiline <- P.option False $ skipEndline >> return True
    contents <- pTemplate
    sep <- P.option mempty $
             do pEnclosed (P.string "sep")
                when multiline $ P.option () skipEndline
                pTemplate
    pEnclosed (P.string "endfor")
    when multiline $ P.option () skipEndline
    return $ Iterate v contents sep

pInterpolate :: (TemplateTarget a, TemplateMonad m)
             => Parser m (Template a)
pInterpolate = do
  pos <- P.getPosition
  -- we don't used pEnclosed here, to get better error messages:
  (closer, var) <- P.try $ do
    cl <- pOpen
    P.skipMany pSpaceOrTab
    v <- pVar
    P.notFollowedBy (P.char '(') -- bare partial
    return (cl, v)
  res <- (P.char ':' *> (pPartialName >>= pPartial (Just var)))
      <|> Iterate var (Interpolate (Variable ["it"] [])) <$> pSep
      <|> return (Interpolate var)
  P.skipMany pSpaceOrTab
  closer
  handleNesting False pos res

pLineEnding :: Monad m => Parser m String
pLineEnding = P.string "\n" <|> P.try (P.string "\r\n") <|> P.string "\r"

pNewlineOrEof :: Monad m => Parser m ()
pNewlineOrEof = () <$ pLineEnding <|> P.eof

handleNesting :: TemplateMonad m
              => Bool -> P.SourcePos -> Template a -> Parser m (Template a)
handleNesting eatEndline pos templ = do
  firstNonspacePos <- firstNonspace <$> P.getState
  let beginline = firstNonspacePos == pos
  endofline <- (True <$ P.lookAhead pNewlineOrEof) <|> pure False
  when (eatEndline && beginline) $ P.optional skipEndline
  mbNested <- nestedCol <$> P.getState
  let toNested t@(Nested{}) = t
      toNested t = case P.sourceColumn pos of
                     1 -> t
                     n | Just n == mbNested -> t
                       | otherwise          -> Nested t
  return $ if beginline && endofline
              then toNested templ
              else templ

pBarePartial :: (TemplateTarget a, TemplateMonad m)
             => Parser m (Template a)
pBarePartial = do
  pos <- P.getPosition
  (closer, fp) <- P.try $ do
    closer <- pOpen
    P.skipMany pSpaceOrTab
    fp <- pPartialName
    return (closer, fp)
  res <- pPartial Nothing fp
  P.skipMany pSpaceOrTab
  closer
  handleNesting True pos res

pPartialName :: TemplateMonad m
             => Parser m FilePath
pPartialName = P.try $ do
  fp <- P.many1 (P.alphaNum <|> P.oneOf ['_','-','.','/','\\'])
  P.string "()"
  return fp

pPartial :: (TemplateTarget a, TemplateMonad m)
         => Maybe Variable -> FilePath -> Parser m (Template a)
pPartial mbvar fp = do
  oldst <- P.getState
  separ <- P.option mempty pSep
  tp <- templatePath <$> P.getState
  let fp' = case takeExtension fp of
               "" -> replaceBaseName tp fp
               _  -> replaceFileName tp fp
  partial <- lift $ removeFinalNewline <$> getPartial fp'
  nesting <- partialNesting <$> P.getState
  t <- if nesting > 50
          then return $ Literal "(loop)"
          else do
            oldInput <- P.getInput
            oldPos <- P.getPosition
            P.setPosition $ P.initialPos fp'
            P.setInput partial
            P.updateState $ \st -> st{ partialNesting = nesting + 1 }
            P.updateState $ \st -> st{ nestedCol = Nothing }
            res' <- pTemplate <* P.eof
            P.updateState $ \st -> st{ partialNesting = nesting }
            P.setInput oldInput
            P.setPosition oldPos
            return res'
  P.putState oldst
  fs <- many pPipe
  case mbvar of
    Just var -> return $ Iterate var (Partial fs t) separ
    Nothing  -> return $ Partial fs t

removeFinalNewline :: Text -> Text
removeFinalNewline t =
  case T.unsnoc t of
    Just (t', '\n') -> t'
    _ -> t

pSep :: (TemplateTarget a, Monad m) => Parser m (Template a)
pSep = do
    P.char '['
    xs <- P.many (P.satisfy (/= ']'))
    P.char ']'
    return $ Literal (fromString xs)

pSpaceOrTab :: Monad m => Parser m Char
pSpaceOrTab = P.satisfy (\c -> c == ' ' || c == '\t')

pComment :: Monad m => Parser m ()
pComment = do
  pos <- P.getPosition
  P.try (P.string "$--")
  P.skipMany (P.satisfy (/='\n'))
  -- If the comment begins in the first column, the line ending
  -- will be consumed; otherwise not.
  when (P.sourceColumn pos == 1) $ () <$ pNewlineOrEof

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
  first <- pIdentPart <|> pIt
  rest <- P.many (P.char '.' *> pIdentPart)
  pipes <- P.many pPipe
  return $ Variable (first:rest) pipes

pPipe :: Monad m => Parser m Pipe
pPipe = do
  P.char '/'
  pipeName <- P.many1 P.letter
  P.notFollowedBy P.letter
  case pipeName of
    "uppercase"  -> return ToUppercase
    "lowercase"  -> return ToLowercase
    "pairs"      -> return ToPairs
    "length"     -> return ToLength
    "alpha"      -> return ToAlpha
    "roman"      -> return ToRoman
    "reverse"    -> return Reverse
    "first"      -> return FirstItem
    "rest"       -> return Rest
    "last"       -> return LastItem
    "allbutlast" -> return AllButLast
    "chomp"      -> return Chomp
    "nowrap"     -> return NoWrap
    "left"       -> Block LeftAligned <$> pBlockWidth <*> pBlockBorders
    "right"      -> Block RightAligned <$> pBlockWidth <*> pBlockBorders
    "center"     -> Block Centered <$> pBlockWidth <*> pBlockBorders
    custom       -> return (Custom custom)

pBlockWidth :: Monad m => Parser m Int
pBlockWidth = P.try (do
  _ <- P.many1 P.space
  ds <- P.many1 P.digit
  case T.decimal (T.pack ds) of
        Right (n,"") -> return n
        _            -> fail "Expected integer parameter for pipe") P.<?>
          "integer parameter for pipe"

pBlockBorders :: Monad m => Parser m Border
pBlockBorders = do
  P.skipMany P.space
  let pBorder = do
        P.char '"'
        cs <- P.many $ (P.noneOf ['"','\\']) <|> (P.char '\\' >> P.anyChar)
        P.char '"'
        P.skipMany P.space
        return $ T.pack cs
  Border <$> P.option mempty pBorder <*> P.option mempty pBorder

pIt :: Monad m => Parser m Text
pIt = fromString <$> P.try (P.string "it")

pIdentPart :: Monad m => Parser m Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let part = first : rest
  guard $ part `notElem` reservedWords
  return $ fromString part

reservedWords :: [String]
reservedWords = ["if","else","endif","elseif","for","endfor","sep","it"]
