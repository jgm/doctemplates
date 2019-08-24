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
      , Indented(..)
      ) where

import Safe (lastMay, initDef)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), Result(..), fromJSON)
import Control.Monad.Identity
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

instance Semigroup Template where
  x <> Empty = x
  Empty <> x = x
  x <> y = Concat x y

instance Monoid Template where
  mappend = (<>)
  mempty = Empty

-- | A variable which may have several parts (@foo.bar.baz@).
newtype Variable = Variable { unVariable :: [Text] }
  deriving (Show, Read, Data, Typeable, Generic, Eq, Ord)

instance Semigroup Variable where
  Variable xs <> Variable ys = Variable (xs <> ys)

instance Monoid Variable where
  mempty = Variable []
  mappend = (<>)

-- | A type to which templates can be rendered.
class Monoid a => TemplateTarget a where
  fromText           :: Text -> a
  toText             :: a -> Text
  removeFinalNewline :: a -> a
  isEmpty            :: a -> Bool
  indent             :: Int -> a -> a

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

instance (DL.HasChars a, IsString a) => TemplateTarget (DL.Doc a) where
  fromText = DL.text . T.unpack
  toText   = T.pack . DL.foldrChar (:) [] . DL.render Nothing
  removeFinalNewline = DL.chomp
  indent = DL.nest
  isEmpty = DL.isEmpty


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
-- a 'Context' or 'Val'.
class ToContext b a where
  toContext :: b -> Context a
  toVal :: b -> Val a

instance TemplateTarget a => ToContext Value a where
  toContext x = case fromJSON x of
                  Success y -> y
                  Error _   -> mempty
  toVal x = case fromJSON x of
                  Success y -> y
                  Error _   -> NullVal

instance TemplateTarget a => ToContext Bool a where
  toContext _ = mempty
  toVal True  = SimpleVal $ fromText "true"
  toVal False = NullVal

instance TemplateTarget a => ToContext Text a where
  toContext _ = mempty
  toVal t = SimpleVal (fromText t)

instance TemplateTarget a => ToContext String a where
  toContext _ = mempty
  toVal s = SimpleVal (fromText (T.pack s))

instance ToContext (Context a) a where
  toContext = id
  toVal     = MapVal

instance ToContext (Val a) a where
  toContext = mempty
  toVal     = id

instance ToContext a a where
  toContext = mempty
  toVal     = SimpleVal

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

instance TemplateTarget a => FromJSON (Context a) where
  parseJSON v = do
    val <- parseJSON v
    case val of
      MapVal o -> return o
      _        -> fail "Not a MapVal"

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

instance TemplateTarget a => ToJSON (Context a) where
  toJSON (Context m) = toJSON m

instance TemplateTarget a => ToJSON (Val a) where
  toJSON NullVal = Null
  toJSON (MapVal m) = toJSON m
  toJSON (ListVal xs) = toJSON xs
  toJSON (SimpleVal t) = toJSON $ toText t

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
                Indented ind -> indent ind $ mconcat vals
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
  getPartial = TIO.readFile
