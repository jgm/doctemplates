{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

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
      , renderTemplateWithCustomPipes
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
      , CustomPipes
      ) where

import Data.Text.Conversions (FromText(..), ToText(..))
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), Result(..), fromJSON)
import Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
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
    | FirstItem
    | LastItem
    | Rest
    | AllButLast
    | Chomp
    | ToAlpha
    | ToRoman
    | NoWrap
    | Block Alignment Int Border
    | Custom String
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
  (HasChars a, ToText a, FromText a)

-- | A 'Context' defines values for template's variables.
newtype Context a = Context { unContext :: M.Map Text (Val a) }
  deriving (Show, Semigroup, Monoid, Traversable, Foldable, Functor,
            Data, Typeable)

-- | A variable value.
data Val a =
    SimpleVal  (Doc a)
  | ListVal    [Val a]
  | MapVal     (Context a)
  | BoolVal    Bool
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
  toVal     = SimpleVal

instance ToContext a b => ToContext a [b] where
  toVal     = ListVal . map toVal

instance {-# OVERLAPPING #-} TemplateTarget [a] => ToContext [a] [a] where
  toVal    = SimpleVal . DL.literal

instance ToContext a b => ToContext a (M.Map Text b) where
  toVal     = MapVal . toContext
  toContext = Context . M.map toVal

instance TemplateTarget a => ToContext a Bool where
  toVal True  = BoolVal True
  toVal False = BoolVal False

instance TemplateTarget a => ToContext a Value where
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

instance {-# OVERLAPPING #-} TemplateTarget [a] => FromContext [a] [a] where
  fromVal (SimpleVal x) = Just (DL.render Nothing x)
  fromVal _             = Nothing

instance FromContext a b => FromContext a [b] where
  fromVal (ListVal  xs) = mapM fromVal xs
  fromVal x             = sequence [fromVal x]

instance TemplateTarget a => FromJSON (Val a) where
  parseJSON v =
    case v of
      Array vec   -> ListVal <$> mapM parseJSON (V.toList vec)
      String t    -> return $ SimpleVal $ DL.literal $ fromText t
      Number n    -> return $ SimpleVal $ fromString $
                              case floatingOrInteger n of
                                  Left (r :: Double)   -> show r
                                  Right (i :: Integer) -> show i
      Bool b      -> return $ BoolVal b
      Object _    -> MapVal . Context <$> parseJSON v
      _           -> return NullVal

instance TemplateTarget a => FromJSON (Context a) where
  parseJSON v = do
    val <- parseJSON v
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
  toJSON (BoolVal b) = toJSON b

mapDoc :: TemplateTarget a => (Doc a -> Doc a) -> Val a -> Val a
mapDoc f val =
  case val of
    SimpleVal d        -> SimpleVal (f d)
    MapVal (Context m) -> MapVal (Context $ M.map (mapDoc f) m)
    ListVal xs         -> ListVal $ map (mapDoc f) xs
    BoolVal b          -> BoolVal b
    NullVal            -> NullVal

mapText :: TemplateTarget a => (Text -> Text) -> Val a -> Val a
mapText f val =
  runIdentity (traverse (return . fromText . f . toText) val)

mapMText :: (TemplateTarget a, Monad m) => (Text -> m Text) -> Val a -> m (Val a)
mapMText f val = traverse (fmap fromText . f . toText) val

applyPipe :: (TemplateTarget a, Monad m) => Pipe -> CustomPipes m -> Val a -> m (Val a)
applyPipe ToLength _cps val = return $ SimpleVal $ fromString . show $ len
  where
   len = case val of
           SimpleVal d        -> T.length . toText $ DL.render Nothing d
           MapVal (Context m) -> M.size m
           ListVal xs         -> length xs
           BoolVal _          -> 0
           NullVal            -> 0
applyPipe ToUppercase _cps val = return $ mapText T.toUpper val
applyPipe ToLowercase _cps val = return $ mapText T.toLower val
applyPipe ToPairs _cps val =
  return $ case val of
    MapVal (Context m) ->
      ListVal $ map toPair $ M.toList m
    ListVal xs         ->
      ListVal $ map toPair $ zip (map (fromString . show) [(1::Int)..]) xs
    _                  -> val
 where
  toPair (k, v) = MapVal $ Context $ M.fromList
                    [ ("key", SimpleVal $ fromString . T.unpack $ k)
                    , ("value", v) ]
applyPipe FirstItem _cps val =
  return $ case val of
    ListVal (x:_) -> x
    _             -> val
applyPipe LastItem _cps val =
  return $ case val of
    ListVal xs@(_:_) -> last xs
    _                -> val
applyPipe Rest _cps val =
  return $ case val of
    ListVal (_:xs) -> ListVal xs
    _              -> val
applyPipe AllButLast _cps val =
  return $ case val of
    ListVal xs@(_:_) -> ListVal (init xs)
    _                -> val
applyPipe Reverse _cps val =
  return $ case val of
    ListVal xs  -> ListVal (reverse xs)
    SimpleVal{} -> mapText T.reverse val
    _           -> val
applyPipe Chomp _cps val = return $ mapDoc DL.chomp val
applyPipe ToAlpha _cps val = return $ mapText toAlpha val
  where toAlpha t =
          case T.decimal t of
            Right (y,"") -> fromString [chr (ord 'a' + (y `mod` 26) - 1)]
            _            -> t
applyPipe ToRoman _cps val = return $ mapText toRoman' val
  where toRoman' t =
         case T.decimal t of
           Right (y,"") -> fromMaybe t (toRoman y)
           _            -> t
applyPipe NoWrap _cps val = return $ mapDoc DL.nowrap val
applyPipe (Block align n border) _cps val =
  let constructor = case align of
                      LeftAligned  -> DL.lblock
                      Centered     -> DL.cblock
                      RightAligned -> DL.rblock
      toBorder y = if T.null y
                      then mempty
                      else DL.vfill (fromText y)
  in return $ case nullToSimple val of
       SimpleVal d -> SimpleVal $
                        toBorder (borderLeft border) <>
                        constructor n d <>
                        toBorder (borderRight border)
       _           -> val
applyPipe (Custom pipe) cps val = case lookup pipe cps of
  Just f -> mapMText f val
  Nothing -> return val

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

applyPipes :: (Monad m, TemplateTarget a) => [Pipe] -> CustomPipes m -> Val a -> m (Val a)
applyPipes fs cps x = foldM (\ih f -> applyPipe f cps ih) x $ reverse fs

multiLookup :: TemplateTarget a => [Text] -> Val a -> Val a
multiLookup [] x = x
multiLookup (t:vs) (MapVal (Context o)) =
  case M.lookup t o of
    Nothing -> NullVal
    Just v' -> multiLookup vs v'
multiLookup _ _ = NullVal

-- The Bool indicates whether it's a true or false value.
data Resolved a = Resolved Bool [Doc a]
   deriving (Show, Read, Data, Typeable, Generic, Eq, Ord,
             Foldable, Traversable, Functor)

instance Semigroup (Resolved a) where
  Resolved b1 x1 <> Resolved b2 x2 = Resolved (b1 || b2) (x1 <> x2)

instance Monoid (Resolved a) where
  mappend = (<>)
  mempty = Resolved False []

resolveVariable :: (TemplateTarget a, Monad m)
                => Variable -> Context a -> CustomPipes m -> m (Resolved a)
resolveVariable v ctx cps = resolveVariable' v (MapVal ctx) cps

resolveVariable' :: (TemplateTarget a, Monad m)
                 => Variable -> Val a -> CustomPipes m -> m (Resolved a)
resolveVariable' v val cps = do
  val' <- applyPipes (varPipes v) cps $ multiLookup (varParts v) val
  case val' of
    ListVal xs    -> mconcat <$> mapM (\val0 -> resolveVariable' mempty val0 cps) xs
    SimpleVal d
      | DL.isEmpty d -> return $ Resolved False []
      | otherwise    -> return $ Resolved True [removeFinalNl d]
    MapVal _      -> return $ Resolved True ["true"]
    BoolVal True  -> return $ Resolved True ["true"]
    BoolVal False -> return $ Resolved False ["false"]
    NullVal       -> return $ Resolved False []

removeFinalNl :: Doc a -> Doc a
removeFinalNl DL.NewLine        = mempty
removeFinalNl DL.CarriageReturn = mempty
removeFinalNl (DL.Concat d1 d2) = d1 <> removeFinalNl d2
removeFinalNl x                 = x

withVariable :: (Monad m, TemplateTarget a)
             => Variable -> Context a -> (Context a -> RenderState m (Doc a)) -> CustomPipes m
             -> RenderState m [Doc a]
withVariable var ctx f cps = do
  val' <- S.lift (applyPipes (varPipes var) cps $ multiLookup (varParts var) (MapVal ctx))
  case val' of
    NullVal     -> return mempty
    ListVal xs  -> mapM (\iterval -> f $ setVarVal iterval) xs
    MapVal ctx' -> (:[]) <$> f (setVarVal (MapVal ctx'))
    _val' -> (:[]) <$> f (setVarVal val')
 where
  setVarVal x =
    addToContext var x $ Context $ M.insert "it" x $ unContext ctx
  addToContext (Variable [] _) _ (Context ctx') = Context ctx'
  addToContext (Variable (v:vs) fs) x (Context ctx') =
    Context $ M.adjust
              (\z -> case z of
                       _ | null vs -> x
                       MapVal m    ->
                         MapVal $ addToContext (Variable vs fs) x m
                       _ -> z) v ctx'

type RenderState m = S.StateT Int m

-- | Render a compiled template in a "context" which provides
-- values for the template's variables.
renderTemplate :: (TemplateTarget a, ToContext a b)
               => Template a -> b -> Doc a
renderTemplate t x = runIdentity (renderTemplateWithCustomPipes t x [])

type CustomPipes m = [(String, Text -> m Text)]

renderTemplateWithCustomPipes :: (TemplateTarget a, ToContext a b, Monad m)
                              => Template a -> b -> CustomPipes m -> m (Doc a)
renderTemplateWithCustomPipes t x cps = S.evalStateT (renderTemp t (toContext x) cps) 0

updateColumn :: (TemplateTarget a, Monad m) => Doc a -> RenderState m (Doc a)
updateColumn x = do
  S.modify $ DL.updateColumn x
  return x

renderTemp :: forall a m. (TemplateTarget a, Monad m)
           => Template a -> Context a -> CustomPipes m -> RenderState m (Doc a)
renderTemp (Literal t) _ _ = updateColumn t
renderTemp (Interpolate v) ctx cps = do
  res <- S.lift (resolveVariable v ctx cps)
  case res of
    Resolved _ xs -> updateColumn (mconcat xs)
renderTemp (Conditional v ift elset) ctx cps = do
  res <- S.lift (resolveVariable v ctx cps)
  case res of
    Resolved False _ -> renderTemp elset ctx cps
    Resolved True _  -> renderTemp ift ctx cps
renderTemp (Iterate v t sep) ctx cps = do
  xs <- withVariable v ctx (\ctx0 -> renderTemp t ctx0 cps) cps
  sep' <- renderTemp sep ctx cps
  return . mconcat . intersperse sep' $ xs
renderTemp (Nested t) ctx cps = do
  n <- S.get
  DL.nest n <$> renderTemp t ctx cps
renderTemp (Partial fs t) ctx cps = do
    val' <- renderTemp t ctx cps
    val'' <- S.lift (applyPipes fs cps (SimpleVal val'))
    return $ case val'' of
      SimpleVal x -> x
      _           -> mempty
renderTemp (Concat t1 t2) ctx cps =
  mappend <$> renderTemp t1 ctx cps <*> renderTemp t2 ctx cps
renderTemp Empty _ _ = return mempty

-- | A 'TemplateMonad' defines a function to retrieve a partial
-- (from the file system, from a database, or using a default
-- value).
class Monad m => TemplateMonad m where
  getPartial  :: FilePath -> m Text

instance TemplateMonad Identity where
  getPartial _  = return mempty

instance TemplateMonad IO where
  getPartial = TIO.readFile
