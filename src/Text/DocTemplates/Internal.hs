{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
      , valueToContext
      , TemplateTarget(..)
      , Template(..)
      , Variable(..)
      , Indented(..)
      ) where

import Data.Aeson (Value(..), ToJSON(..))
import Control.Monad.Identity
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String (IsString(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger)
import Data.Semigroup (Semigroup, (<>))
import Data.List (intersperse)

-- | Determines whether an interpolated variable is rendered with
-- indentation.
data Indented = Indented !Int | Unindented
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

-- | A template.
data Template =
       Interpolate Indented Variable
     | Conditional Variable Template Template
     | Iterate Variable Template Template
     | Partial Template
     | Literal Text
     | Concat Template Template
     | Empty
     deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Template where
  x <> Empty = x
  Empty <> x = x
  x <> y = Concat x y

instance Monoid Template where
  mempty = Empty
#else
instance Monoid Template where
  mappend x Empty = x
  mappend Empty x = x
  mappend x y = Concat x y
  mempty = Empty
#endif

-- | A variable which may have several parts (@foo.bar.baz@).
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

-- | A type to which templates can be rendered.
class Monoid a => TemplateTarget a where
  fromText           :: Text -> a      -- | Convert from text.
  removeFinalNewline :: a -> a         -- | Remove final newline.
  isEmpty            :: a -> Bool      -- | Renders empty.
  nested             :: Int -> a -> a  -- | Nest (indent).

instance TemplateTarget Text where
  fromText   = id
  removeFinalNewline t =
    case T.unsnoc t of
      Just (t', '\n') -> t'
      _               -> t
  isEmpty    = T.null
  nested 0   = id
  nested ind = T.intercalate ("\n" <> T.replicate ind " ") . T.lines

-- | A 'Context' defines values for template's variables.
newtype Context a = Context { unContext :: M.Map Text (Val a) }
  deriving (Show, Semigroup, Monoid, Traversable, Foldable, Functor)

-- | A variable value.
data Val a =
    SimpleVal  a
  | ListVal    [Val a]
  | MapVal     (Context a)
  | NullVal
  deriving (Show, Traversable, Foldable, Functor)

-- | The 'ToContext' class provides automatic conversion to
-- a 'Context'.
class ToContext b a where
  toContext :: b -> Context a

instance TemplateTarget a => ToContext Value a where
  toContext = valueToContext

instance ToContext (Context a) a where
  toContext = id

-- | The 'FromContext' class provides functions for extracting
-- values from 'Val' and 'Context'.
class FromContext a b where
  fromVal :: Val a -> Maybe b
  lookupContext :: Text -> Context a -> Maybe b
  lookupContext t (Context m) = M.lookup t m >>= fromVal

instance FromContext a (Val a) where
  fromVal = Just

instance FromContext a a where
  fromVal (SimpleVal x) = Just x
  fromVal _             = Nothing

instance FromContext a [a] where
  fromVal (SimpleVal x) = Just [x]
  fromVal (ListVal  xs) = mapM fromVal xs
  fromVal _             = Nothing

valueToVal :: (TemplateTarget a, ToJSON b) => b -> Val a
valueToVal x =
  case toJSON x of
    Array vec   -> ListVal $ map valueToVal $ V.toList vec
    String t    -> SimpleVal $ fromText t
    Number n    -> SimpleVal $ fromText . fromString $
                           case floatingOrInteger n of
                                Left (r :: Double)   -> show r
                                Right (i :: Integer) -> show i
    Bool True   -> SimpleVal $ fromText "true"
    Object o    -> MapVal $ Context $ M.fromList $ H.toList $ H.map valueToVal o
    _           -> NullVal

-- | Converts an Aeson 'Value' to a 'Context'.
valueToContext :: (TemplateTarget a, ToJSON b) => b -> Context a
valueToContext val =
  case valueToVal val of
    MapVal o -> o
    _        -> Context mempty


multiLookup :: [Text] -> Val a -> Val a
multiLookup [] x = x
multiLookup (v:vs) (MapVal (Context o)) =
  case M.lookup v o of
    Nothing -> NullVal
    Just v' -> multiLookup vs v'
multiLookup _ _ = NullVal

resolveVariable :: TemplateTarget a => Variable -> Context a -> [a]
resolveVariable v ctx = resolveVariable' v (MapVal ctx)

resolveVariable' :: TemplateTarget a => Variable -> Val a -> [a]
resolveVariable' v val =
  case multiLookup (unVariable v) val of
    ListVal xs    -> concatMap (resolveVariable' mempty) xs
    SimpleVal t
      | isEmpty t -> []
      | otherwise -> [removeFinalNewline t]
    MapVal _      -> [fromText "true"]
    NullVal       -> []

withVariable :: TemplateTarget a
             => Variable -> Context a -> (Context a -> a) -> [a]
withVariable  v ctx f =
  case multiLookup (unVariable v) (MapVal ctx) of
    NullVal     -> mempty
    ListVal xs  -> map (\iterval -> f $
                    Context $ M.insert "it" iterval $ unContext ctx) xs
    val' -> [f $ Context $ M.insert "it" val' $ unContext ctx]

-- | Render a compiled template in a "context" which provides
-- values for the template's variables.
renderTemplate :: (TemplateTarget a, ToContext b a)
               => Template -> b -> a
renderTemplate t = renderTemp t . toContext

renderTemp :: forall a . TemplateTarget a
           => Template -> Context a -> a
renderTemp (Literal t) _ = fromText t
renderTemp (Interpolate indented v) ctx =
  let vals = resolveVariable v ctx
   in if null vals
         then mempty
         else case indented of
                Indented ind -> nested ind $ mconcat vals
                _            -> mconcat vals
renderTemp (Conditional v ift elset) ctx =
  let res = resolveVariable v ctx
   in case res of
        [] -> renderTemp elset ctx
        _  -> renderTemp ift ctx
renderTemp (Iterate v t sep) ctx =
  let sep' = renderTemp sep ctx
   in mconcat . intersperse sep' $ withVariable v ctx (renderTemp t)
renderTemp (Partial t) ctx = renderTemp t ctx
renderTemp (Concat t1 t2) ctx =
  mappend (renderTemp t1 ctx) (renderTemp t2 ctx)
renderTemp Empty _ = mempty

-- | A 'TemplateMonad' defines a function to retrieve a partial
-- (from the file system, from a database, or using a default
-- value).
class Monad m => TemplateMonad m where
  getPartial  :: FilePath -> m Text

instance TemplateMonad Identity where
  getPartial _  = return mempty

instance TemplateMonad IO where
  getPartial s  = TIO.readFile s
