{-# LANGUAGE CPP #-}
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
      , TemplateTarget(..)
      , Template(..)
      , Variable(..)
      , Filter(..)
      ) where

import Safe (lastMay, initDef)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), Result(..), fromJSON)
import Data.YAML (ToYAML(..), FromYAML(..), Node(..), Scalar(..))
import Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
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
import Data.List (intersperse, intercalate)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

-- | A template.
data Template =
       Interpolate Variable
     | Conditional Variable Template Template
     | Iterate Variable Template Template
     | Nested Template
     | Partial Template
     | Literal Text
     | Concat Template Template
     | BreakingSpace
     | Empty
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

instance Semigroup Template where
  x <> Empty = x
  Empty <> x = x
  x <> y = Concat x y

instance Monoid Template where
  mappend = (<>)
  mempty = Empty

data Filter =
      ToPairs
    | ToUppercase
    | ToLowercase
    | ToLength
    deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

-- | A variable which may have several parts (@foo.bar.baz@).
data Variable =
  Variable
    { varParts   :: [Text]
    , varFilters :: [Filter]
    }
  deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

instance Semigroup Variable where
  Variable xs fs <> Variable ys gs = Variable (xs <> ys) (fs <> gs)

instance Monoid Variable where
  mempty = Variable mempty mempty
  mappend = (<>)

-- | A type to which templates can be rendered.
class Monoid a => TemplateTarget a where
  fromText           :: Text -> a
  toText             :: a -> Text
  removeFinalNewline :: a -> a
  isEmpty            :: a -> Bool
  indent             :: Int -> a -> a
  breakingSpace      :: a

instance TemplateTarget Text where
  fromText   = id
  toText     = id
  removeFinalNewline t =
    case T.unsnoc t of
      Just (t', '\n') -> t'
      _               -> t
  isEmpty    = T.null
  indent 0   = id
  indent ind = T.intercalate ("\n" <> T.replicate ind " ") . T.lines
  breakingSpace = " "

instance TemplateTarget TL.Text where
  fromText   = TL.fromStrict
  toText     = TL.toStrict
  removeFinalNewline t =
    case TL.unsnoc t of
      Just (t', '\n') -> t'
      _               -> t
  isEmpty    = TL.null
  indent 0   = id
  indent ind = TL.intercalate ("\n" <> TL.replicate (fromIntegral ind) " ")
               . TL.lines
  breakingSpace = " "

instance TemplateTarget String where
  fromText   = T.unpack
  toText     = T.pack
  removeFinalNewline t =
    case lastMay t of
      Just '\n'       -> initDef t t
      _               -> t
  isEmpty    = null
  indent 0   = id
  indent ind = intercalate ("\n" <> replicate ind ' ') . lines
  breakingSpace = " "

instance (DL.HasChars a, IsString a, Eq a)
    => TemplateTarget (DL.Doc a) where
  fromText = DL.text . T.unpack
  toText   = T.pack . DL.foldrChar (:) [] . DL.render Nothing
  removeFinalNewline = DL.chomp
  indent = DL.nest
  isEmpty (DL.Empty)      = True
  isEmpty (DL.Text 0 _)   = True
  isEmpty (DL.Concat x y) = isEmpty x && isEmpty y
  isEmpty _               = False
  breakingSpace  = DL.space

-- | A 'Context' defines values for template's variables.
newtype Context a = Context { unContext :: M.Map Text (Val a) }
  deriving (Show, Semigroup, Monoid, Traversable, Foldable, Functor,
            Data, Typeable)

-- | A variable value.
data Val a =
    SimpleVal  a
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

instance ToContext a a where
  toVal     = SimpleVal

-- This is needed because otherwise the compiler tries to
-- match on ToContext a [b], with a = b = Char, even though
-- we don't have ToContext Char Char.  I don't understand why.
instance {-# OVERLAPS #-} ToContext String String where
  toVal t   = SimpleVal t

instance ToContext a b => ToContext a [b] where
  toVal     = ListVal . map toVal

instance ToContext a b => ToContext a (M.Map Text b) where
  toVal     = MapVal . toContext
  toContext = Context . M.map toVal

instance TemplateTarget a => ToContext a Value where
  toContext x = case fromJSON x of
                  Success y -> y
                  Error _   -> mempty
  toVal x = case fromJSON x of
                  Success y -> y
                  Error _   -> NullVal

instance TemplateTarget a => ToContext a Bool where
  toVal True  = SimpleVal $ fromText "true"
  toVal False = NullVal

instance (TemplateTarget a, DL.HasChars a) => ToContext (DL.Doc a) a where
  toVal t   = SimpleVal $ DL.Text (DL.realLength t) t

-- | The 'FromContext' class provides functions for extracting
-- values from 'Val' and 'Context'.
class FromContext a b where
  fromVal :: Val a -> Maybe b
  lookupContext :: Text -> Context a -> Maybe b
  lookupContext t (Context m) = M.lookup t m >>= fromVal

instance TemplateTarget a => FromContext a (Val a) where
  fromVal = Just

instance TemplateTarget a => FromContext a a where
  fromVal (SimpleVal x) = Just x
  fromVal _             = Nothing

-- This is needed because otherwise the compiler tries to
-- match on FromContext a [b], with a = b = Char, even though
-- we don't have FromContext Char Char.  I don't understand why.
instance {-# OVERLAPS #-} FromContext String String where
  fromVal (SimpleVal x) = Just x
  fromVal _             = Nothing

instance FromContext a b => FromContext a [b] where
  fromVal (ListVal  xs) = mapM fromVal xs
  fromVal x             = sequence [fromVal x]

instance TemplateTarget a => FromJSON (Val a) where
  parseJSON v =
    case v of
      Array vec   -> ListVal <$> mapM parseJSON (V.toList vec)
      String t    -> return $ SimpleVal $ fromText t
      Number n    -> return $ SimpleVal $ fromText . fromString $
                              case floatingOrInteger n of
                                  Left (r :: Double)   -> show r
                                  Right (i :: Integer) -> show i
      Bool True   -> return $ SimpleVal $ fromText "true"
      Object o    -> MapVal . Context . M.fromList . H.toList <$>
                       mapM parseJSON o
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
  toJSON (SimpleVal t) = toJSON $ toText t

applyFilter :: TemplateTarget a => Filter -> Val a -> Val a
applyFilter ToLength val = SimpleVal $ fromText . T.pack . show $ len
  where
   len = case val of
           SimpleVal t        -> T.length . toText $ t
           MapVal (Context m) -> M.size m
           ListVal xs         -> length xs
           NullVal            -> 0
applyFilter ToUppercase val =
  case val of
    SimpleVal t -> SimpleVal $ fromText . T.toUpper . toText $ t
    _           -> val
applyFilter ToLowercase val =
  case val of
    SimpleVal t -> SimpleVal $ fromText . T.toLower . toText $ t
    _           -> val
applyFilter ToPairs val =
  case val of
    MapVal (Context m) ->
      ListVal $ map toPair $ M.toList m
    ListVal xs         ->
      ListVal $ map toPair $ zip (map (T.pack . show) [(1::Int)..]) xs
    _                  -> val
 where
  toPair (k, v) = MapVal $ Context $ M.fromList
                    [ ("key", SimpleVal (fromText k))
                    , ("value", v) ]

multiLookup :: TemplateTarget a => [Filter] -> [Text] -> Val a -> Val a
multiLookup fs [] x = foldr applyFilter x fs
multiLookup fs (t:vs) (MapVal (Context o)) =
  case M.lookup t o of
    Nothing -> NullVal
    Just v' -> multiLookup fs vs v'
multiLookup _ _ _ = NullVal

instance TemplateTarget a => FromYAML (Val a) where
  parseYAML v =
    case v of
      Mapping _ _ m -> MapVal . Context . M.fromList <$>
                           mapM (\(key, val) -> do
                                  val' <- parseYAML val
                                  key' <- parseYAML key
                                  return (key', val')) (M.toList m)
      Sequence _ _ xs -> ListVal <$> mapM parseYAML xs
      Scalar _ (SStr t) -> return $ SimpleVal $ fromText t
      Scalar _ (SFloat n) -> return $ SimpleVal $ fromText . fromString . show $ n
      Scalar _ (SInt n) -> return $ SimpleVal $ fromText . fromString . show $ n
      Scalar _ (SBool True) -> return $ SimpleVal $ fromText "true"
      _           -> return NullVal

instance TemplateTarget a => FromYAML (Context a) where
  parseYAML v = do
    val <- parseYAML v
    case val of
      MapVal o -> return o
      _        -> fail "Expecting MapVal"

instance TemplateTarget a => ToYAML (Context a) where
  toYAML (Context m) = toYAML m

instance TemplateTarget a => ToYAML (Val a) where
  toYAML NullVal = toYAML (Nothing :: Maybe Text)
  toYAML (MapVal m) = toYAML m
  toYAML (ListVal xs) = toYAML xs
  toYAML (SimpleVal t) = toYAML $ toText t

resolveVariable :: TemplateTarget a => Variable -> Context a -> [a]
resolveVariable v ctx = resolveVariable' v (MapVal ctx)

resolveVariable' :: TemplateTarget a => Variable -> Val a -> [a]
resolveVariable' v val =
  case multiLookup (varFilters v) (varParts v) val of
    ListVal xs    -> concatMap (resolveVariable' mempty) xs
    SimpleVal t
      | isEmpty t -> []
      | otherwise -> [removeFinalNewline t]
    MapVal _      -> [fromText "true"]
    NullVal       -> []

withVariable :: (Monad m, TemplateTarget a)
             => Variable -> Context a -> (Context a -> m a) -> m [a]
withVariable  v ctx f =
  case multiLookup (varFilters v) (varParts v) (MapVal ctx) of
    NullVal     -> return mempty
    ListVal xs  -> mapM (\iterval -> f $
                    Context $ M.insert "it" iterval $ unContext ctx) xs
    val' -> (:[]) <$> f (Context $ M.insert "it" val' $ unContext ctx)

-- | Render a compiled template in a "context" which provides
-- values for the template's variables.
renderTemplate :: (TemplateTarget a, ToContext a b)
               => Template -> b -> a
renderTemplate t x = S.evalState (renderTemp t (toContext x)) 0

updateColumn :: TemplateTarget a => a -> S.State Int a
updateColumn x = do
  let t = toText x
  let (prefix, remainder) = T.breakOnEnd "\n" t
  if T.null prefix
     then S.modify (+ T.length remainder)
     else S.put (T.length remainder)
  return x

renderTemp :: forall a . TemplateTarget a
           => Template -> Context a -> S.State Int a
renderTemp (Literal t) _ = updateColumn $ fromText t
renderTemp BreakingSpace _ = updateColumn breakingSpace
renderTemp (Interpolate v) ctx = updateColumn $ mconcat $ resolveVariable v ctx
renderTemp (Conditional v ift elset) ctx =
  let res = resolveVariable v ctx
   in case res of
        [] -> renderTemp elset ctx
        _  -> renderTemp ift ctx
renderTemp (Iterate v t sep) ctx = do
  sep' <- renderTemp sep ctx
  mconcat . intersperse sep' <$> withVariable v ctx (renderTemp t)
renderTemp (Nested t) ctx = do
  n <- S.get
  indent n <$> renderTemp t ctx
renderTemp (Partial t) ctx = renderTemp t ctx
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
