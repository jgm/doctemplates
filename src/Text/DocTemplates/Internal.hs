{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

{- |
   Module      : Text.DocTemplates.Internal
   Copyright   : Copyright (C) 2009-2019 John MacFarlane
   License     : BSD3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}

module Text.DocTemplates.Internal
      ( renderTemplate
      , TemplateMonad(..)
      , Context(..)
      , Val(..)
      , ToContext(..)
      , FromContext(..)
      , TemplateTarget
      , Template(..)
      , Variable(..)
      , Pipe(..)
      , Alignment(..)
      , Border(..)
      ) where

import Data.Text.Conversions (FromText(..), ToText(..))
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), Result(..), fromJSON)
import Data.YAML (ToYAML(..), FromYAML(..), Node(..), Scalar(..))
import Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import Data.Char (chr, ord)
import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.DocLayout (Doc, HasChars)
import qualified Text.DocLayout as DL
import Data.String (IsString(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger)
import Data.List (intersperse)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

-- | A template.
data Template a =
       Interpolate Variable
     | Conditional Variable (Template a) (Template a)
     | Iterate Variable (Template a) (Template a)
     | Nested (Template a)
     | Partial [Pipe] (Template a)
     | Literal (Doc a)
     | Concat (Template a) (Template a)
     | Empty
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord,
               Foldable, Traversable, Functor)

instance Semigroup a => Semigroup (Template a) where
  x <> Empty = x
  Empty <> x = x
  x <> y = Concat x y

instance Semigroup a => Monoid (Template a) where
  mappend = (<>)
  mempty = Empty

data Pipe =
      ToPairs
    | ToUppercase
    | ToLowercase
    | ToLength
    | Reverse
    | First
    | Last
    | Rest
    | AllButLast
    | Chomp
    | ToAlpha
    | ToRoman
    | NoWrap
    | Block Alignment Int Border
    deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

data Alignment =
      LeftAligned
    | Centered
    | RightAligned
    deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

data Border = Border
     { borderLeft  :: Text
     , borderRight :: Text
     }
    deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

-- | A variable which may have several parts (@foo.bar.baz@).
data Variable =
  Variable
    { varParts   :: [Text]
    , varPipes   :: [Pipe]
    }
  deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

instance Semigroup Variable where
  Variable xs fs <> Variable ys gs = Variable (xs <> ys) (fs <> gs)

instance Monoid Variable where
  mempty = Variable mempty mempty
  mappend = (<>)

type TemplateTarget a =
  (Monoid a, IsString a, HasChars a, ToText a, FromText a)

-- | A 'Context' defines values for template's variables.
newtype Context a = Context { unContext :: M.Map Text (Val a) }
  deriving (Show, Semigroup, Monoid, Traversable, Foldable, Functor,
            Data, Typeable)

-- | A variable value.
data Val a =
    SimpleVal  (Doc a)
  | ListVal    [Val a]
  | MapVal     (Context a)
  | NullVal
  deriving (Show, Traversable, Foldable, Functor, Data, Typeable)

-- | The 'ToContext' class provides automatic conversion to
-- a 'Context' or 'Val'.
class ToContext a b where
  toContext :: b -> Context a
  toContext x = case toVal x of
                  MapVal c -> c
                  _        -> mempty
  toVal     :: b -> Val a

instance ToContext a (Context a) where
  toContext = id
  toVal     = MapVal

instance ToContext a (Val a) where
  toVal     = id

instance TemplateTarget a => ToContext a a where
  toVal     = SimpleVal . DL.literal

instance ToContext a a => ToContext a (Doc a) where
  toVal    = SimpleVal

-- This is needed because otherwise the compiler tries to
-- match on ToContext a [b], with a = b = Char, even though
-- we don't have ToContext Char Char.  I don't understand why.
instance {-# OVERLAPS #-} ToContext String String where
  toVal    = SimpleVal . DL.literal

instance {-# OVERLAPS #-} ToContext String (Doc String) where
  toVal    = SimpleVal

instance ToContext a b => ToContext a [b] where
  toVal     = ListVal . map toVal

instance ToContext a b => ToContext a (M.Map Text b) where
  toVal     = MapVal . toContext
  toContext = Context . M.map toVal

instance TemplateTarget a => ToContext a Bool where
  toVal True  = SimpleVal "true"
  toVal False = NullVal

instance (IsString a, TemplateTarget a) => ToContext a Value where
  toContext x = case fromJSON x of
                  Success y -> y
                  Error _   -> mempty
  toVal x = case fromJSON x of
                  Success y -> y
                  Error _   -> NullVal

-- | The 'FromContext' class provides functions for extracting
-- values from 'Val' and 'Context'.
class FromContext a b where
  fromVal :: Val a -> Maybe b
  lookupContext :: Text -> Context a -> Maybe b
  lookupContext t (Context m) = M.lookup t m >>= fromVal

instance TemplateTarget a => FromContext a (Val a) where
  fromVal = Just

instance TemplateTarget a => FromContext a (Doc a) where
  fromVal (SimpleVal x) = Just x
  fromVal _             = Nothing

instance TemplateTarget a => FromContext a a where
  fromVal (SimpleVal x) = Just (DL.render Nothing x)
  fromVal _             = Nothing

-- This is needed because otherwise the compiler tries to
-- match on FromContext a [b], with a = b = Char, even though
-- we don't have FromContext Char Char.  I don't understand why.
instance {-# OVERLAPS #-} FromContext String String where
  fromVal (SimpleVal x) = Just (DL.render Nothing x)
  fromVal _             = Nothing

instance FromContext a b => FromContext a [b] where
  fromVal (ListVal  xs) = mapM fromVal xs
  fromVal x             = sequence [fromVal x]

instance (IsString a, TemplateTarget a) => FromJSON (Val a) where
  parseJSON v =
    case v of
      Array vec   -> ListVal <$> mapM parseJSON (V.toList vec)
      String t    -> return $ SimpleVal $ DL.literal $ fromText t
      Number n    -> return $ SimpleVal $ fromString $
                              case floatingOrInteger n of
                                  Left (r :: Double)   -> show r
                                  Right (i :: Integer) -> show i
      Bool True   -> return $ SimpleVal "true"
      Object o    -> MapVal . Context . M.fromList . H.toList <$>
                       mapM parseJSON o
      _           -> return NullVal

instance (IsString a, TemplateTarget a) => FromJSON (Context a) where
  parseJSON v = do
    val <- parseJSON v
    case val of
      MapVal o -> return o
      _        -> fail "Expecting MapVal"

instance TemplateTarget a => FromYAML (Val a) where
  parseYAML v =
    case v of
      Mapping _ _ m -> MapVal . Context . M.fromList <$>
                           mapM (\(key, val) -> do
                                  val' <- parseYAML val
                                  key' <- parseYAML key
                                  return (key', val')) (M.toList m)
      Sequence _ _ xs -> ListVal <$> mapM parseYAML xs
      Scalar _ (SStr t) -> return $ SimpleVal $ fromString . fromText $ t
      Scalar _ (SFloat n) -> return $ SimpleVal $ fromString . show $ n
      Scalar _ (SInt n) -> return $ SimpleVal $ fromString . show $ n
      Scalar _ (SBool True) -> return $ SimpleVal "true"
      _           -> return NullVal

instance (IsString a, TemplateTarget a) => FromYAML (Context a) where
  parseYAML v = do
    val <- parseYAML v
    case val of
      MapVal o -> return o
      _        -> fail "Expecting MapVal"

instance TemplateTarget a => ToJSON (Context a) where
  toJSON (Context m) = toJSON m

instance TemplateTarget a => ToJSON (Val a) where
  toJSON NullVal = Null
  toJSON (MapVal m) = toJSON m
  toJSON (ListVal xs) = toJSON xs
  toJSON (SimpleVal d) = toJSON $ toText $ DL.render Nothing d

instance TemplateTarget a => ToYAML (Context a) where
  toYAML (Context m) = toYAML m

instance TemplateTarget a => ToYAML (Val a) where
  toYAML NullVal = toYAML (Nothing :: Maybe Text)
  toYAML (MapVal m) = toYAML m
  toYAML (ListVal xs) = toYAML xs
  toYAML (SimpleVal d) = toYAML $ toText $ DL.render Nothing d

mapDoc :: TemplateTarget a => (Doc a -> Doc a) -> Val a -> Val a
mapDoc f val =
  case val of
    SimpleVal d        -> SimpleVal (f d)
    MapVal (Context m) -> MapVal (Context $ M.map (mapDoc f) m)
    ListVal xs         -> ListVal $ map (mapDoc f) xs
    NullVal            -> NullVal

mapText :: TemplateTarget a => (Text -> Text) -> Val a -> Val a
mapText f val =
  runIdentity (traverse (return . fromText . f . toText) val)

applyPipe :: TemplateTarget a => Pipe -> Val a -> Val a
applyPipe ToLength val = SimpleVal $ fromString . show $ len
  where
   len = case val of
           SimpleVal d        -> T.length . toText $ DL.render Nothing d
           MapVal (Context m) -> M.size m
           ListVal xs         -> length xs
           NullVal            -> 0
applyPipe ToUppercase val = mapText T.toUpper val
applyPipe ToLowercase val = mapText T.toLower val
applyPipe ToPairs val =
  case val of
    MapVal (Context m) ->
      ListVal $ map toPair $ M.toList m
    ListVal xs         ->
      ListVal $ map toPair $ zip (map (fromString . show) [(1::Int)..]) xs
    _                  -> val
 where
  toPair (k, v) = MapVal $ Context $ M.fromList
                    [ ("key", SimpleVal $ fromString . T.unpack $ k)
                    , ("value", v) ]
applyPipe First val =
  case val of
    ListVal (x:_) -> x
    _             -> val
applyPipe Last val =
  case val of
    ListVal xs@(_:_) -> last xs
    _                -> val
applyPipe Rest val =
  case val of
    ListVal (_:xs) -> ListVal xs
    _              -> val
applyPipe AllButLast val =
  case val of
    ListVal xs@(_:_) -> ListVal (init xs)
    _                -> val
applyPipe Reverse val =
  case val of
    ListVal xs  -> ListVal (reverse xs)
    SimpleVal{} -> mapText T.reverse val
    _           -> val
applyPipe Chomp val = mapDoc DL.chomp val
applyPipe ToAlpha val = mapText toAlpha val
  where toAlpha t =
          case T.decimal t of
            Right (y,"") -> fromString [chr (ord 'a' + (y `mod` 26) - 1)]
            _            -> t
applyPipe ToRoman val = mapText toRoman' val
  where toRoman' t =
         case T.decimal t of
           Right (y,"") -> maybe t id (toRoman y)
           _            -> t
applyPipe NoWrap val = mapDoc DL.nowrap val
applyPipe (Block align n border) val =
  let constructor = case align of
                      LeftAligned  -> DL.lblock
                      Centered     -> DL.cblock
                      RightAligned -> DL.rblock
      toBorder y = if T.null y
                      then mempty
                      else DL.vfill (fromText y)
  in case nullToSimple val of
       SimpleVal d -> SimpleVal $
                        toBorder (borderLeft border) <>
                        constructor n d <>
                        toBorder (borderRight border)
       _           -> val

nullToSimple :: Monoid a => Val a -> Val a
nullToSimple NullVal = SimpleVal mempty
nullToSimple x = x

-- | Convert number 0 < x < 4000 to lowercase roman numeral.
toRoman :: Int -> Maybe Text
toRoman x
  | x >= 1000
  , x < 4000  = ("m" <>) <$> toRoman (x - 1000)
  | x >= 900  = ("cm" <>) <$> toRoman (x - 900)
  | x >= 500  = ("d" <>) <$> toRoman (x - 500)
  | x >= 400  = ("cd" <>) <$> toRoman (x - 400)
  | x >= 100  = ("c" <>) <$> toRoman (x - 100)
  | x >= 90   = ("xc" <>) <$> toRoman (x - 90)
  | x >= 50   = ("l" <>) <$> toRoman (x - 50)
  | x >= 40   = ("xl" <>) <$> toRoman (x - 40)
  | x >= 10   = ("x" <>) <$> toRoman (x - 10)
  | x == 9    = return "ix"
  | x >= 5    = ("v" <>) <$> toRoman (x - 5)
  | x == 4    = return "iv"
  | x >= 1    = ("i" <>) <$> toRoman (x - 1)
  | x == 0    = return ""
  | otherwise = Nothing

applyPipes :: TemplateTarget a => [Pipe] -> Val a -> Val a
applyPipes fs x = foldr applyPipe x $ reverse fs

multiLookup :: TemplateTarget a => [Text] -> Val a -> Val a
multiLookup [] x = x
multiLookup (t:vs) (MapVal (Context o)) =
  case M.lookup t o of
    Nothing -> NullVal
    Just v' -> multiLookup vs v'
multiLookup _ _ = NullVal

resolveVariable :: TemplateTarget a => Variable -> Context a -> [Doc a]
resolveVariable v ctx = resolveVariable' v (MapVal ctx)

resolveVariable' :: TemplateTarget a => Variable -> Val a -> [Doc a]
resolveVariable' v val =
  case applyPipes (varPipes v) $ multiLookup (varParts v) val of
    ListVal xs    -> concatMap (resolveVariable' mempty) xs
    SimpleVal d
      | DL.isEmpty d -> []
      | otherwise    -> [removeFinalNl d]
    MapVal _      -> ["true"]
    NullVal       -> []

removeFinalNl :: Doc a -> Doc a
removeFinalNl DL.NewLine        = mempty
removeFinalNl DL.CarriageReturn = mempty
removeFinalNl (DL.Concat d1 d2) = d1 <> removeFinalNl d2
removeFinalNl x                 = x

withVariable :: (Monad m, TemplateTarget a)
             => Variable -> Context a -> (Context a -> m (Doc a))
             -> m [Doc a]
withVariable  v ctx f =
  case applyPipes (varPipes v) $ multiLookup (varParts v) (MapVal ctx) of
    NullVal     -> return mempty
    ListVal xs  -> mapM (\iterval -> f $
                    Context $ M.insert "it" iterval $ unContext ctx) xs
    MapVal ctx' -> (:[]) <$> f
                    (Context $ M.insert "it" (MapVal ctx') $ unContext ctx)
    val' -> (:[]) <$> f (Context $ M.insert "it" val' $ unContext ctx)

type RenderState = S.State Int

-- | Render a compiled template in a "context" which provides
-- values for the template's variables.
renderTemplate :: (TemplateTarget a, ToContext a b)
               => Template a -> b -> Doc a
renderTemplate t x = S.evalState (renderTemp t (toContext x)) 0

updateColumn :: TemplateTarget a => Doc a -> RenderState (Doc a)
updateColumn x = do
  S.modify $ DL.updateColumn x
  return x

renderTemp :: forall a . TemplateTarget a
           => Template a -> Context a -> RenderState (Doc a)
renderTemp (Literal t) _ = updateColumn $ t
renderTemp (Interpolate v) ctx = updateColumn $ mconcat $ resolveVariable v ctx
renderTemp (Conditional v ift elset) ctx =
  let res = resolveVariable v ctx
   in case res of
        [] -> renderTemp elset ctx
        _  -> renderTemp ift ctx
renderTemp (Iterate v t sep) ctx = do
  xs <- withVariable v ctx (renderTemp t)
  sep' <- renderTemp sep ctx
  return . mconcat . intersperse sep' $ xs
renderTemp (Nested t) ctx = do
  n <- S.get
  DL.nest n <$> renderTemp t ctx
renderTemp (Partial fs t) ctx = do
    val' <- renderTemp t ctx
    return $ case applyPipes fs (SimpleVal val') of
      SimpleVal x -> x
      _           -> mempty
renderTemp (Concat t1 t2) ctx =
  mappend <$> renderTemp t1 ctx <*> renderTemp t2 ctx
renderTemp Empty _ = return mempty

-- | A 'TemplateMonad' defines a function to retrieve a partial
-- (from the file system, from a database, or using a default
-- value).
class Monad m => TemplateMonad m where
  getPartial  :: FilePath -> m Text

instance TemplateMonad Identity where
  getPartial _  = return mempty

instance TemplateMonad IO where
  getPartial = TIO.readFile
