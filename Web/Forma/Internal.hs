-- |
-- Module      :  Web.Forma.Internal
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parts of this module not exposed from Web.Forma are subject to change
-- without notice and with no regard for backwards compatibility. Use at
-- your own risk.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Web.Forma.Internal where

import Control.Applicative
import Data.Aeson
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Text (Text)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NE
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T


----------------------------------------------------------------------------
-- Types

-- | Result of parsing. @names@ is the collection of allowed field names,
-- @e@ is the type of validation errors, and @a@ is the type of parsing
-- result.

data FormResult (names :: [Symbol]) e a
  = ParsingFailed (Maybe (FieldName names)) Text
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error. The first component specifies path to a problematic
    -- field and the second component is the text of error message.
  | ValidationFailed (Map (FieldName names) e)
    -- ^ Validation of a field failed. This is also fatal but we still try
    -- to validate other branches (fields) to collect as many validation
    -- errors as possible.
  | Succeeded a
    -- ^ Success, we've got a result to return.
  deriving (Eq, Show, Functor)

instance (ToJSON e, ToJSON a) => ToJSON (FormResult names e a) where
  toJSON = \case
    ParsingFailed path msg ->
      f (Just (path, msg)) Nothing Nothing
    ValidationFailed verr ->
      f Nothing (Just verr) Nothing
    Succeeded x ->
      f Nothing Nothing (Just x)
    where
      f :: Maybe (Maybe (FieldName names), Text)
        -> Maybe (Map (FieldName names) e)
        -> Maybe a
        -> Value
      f perr verr result = object
        [ "parse_error" .=
          case perr of
            Nothing -> Null
            Just (path, msg) -> object
              [ "field"   .= path
              , "message" .= msg
              ]
        , "field_errors" .=
          let g (fieldName, err) = showFieldName fieldName .= err
          in maybe (Object HM.empty) (object . fmap g . M.toAscList) verr
        , "result" .= result
        ]

instance Applicative (FormResult names e) where
  pure                                            = Succeeded
  (ParsingFailed l msg) <*> _                     = ParsingFailed l msg
  (ValidationFailed _)  <*> (ParsingFailed l msg) = ParsingFailed l msg
  (ValidationFailed e0) <*> (ValidationFailed e1) = ValidationFailed (e0 <> e1)
  (ValidationFailed e)  <*> Succeeded _           = ValidationFailed e
  Succeeded _           <*> (ParsingFailed l msg) = ParsingFailed l msg
  Succeeded _           <*> (ValidationFailed e)  = ValidationFailed e
  Succeeded f           <*> Succeeded x           = Succeeded (f x)

-- | The type represents the parser that you can run on a 'Value' with the
-- help of 'runForm'. The only way for the user of the library to create a
-- parser is via the 'field' function and its friends, see below. Users can
-- combine existing parsers using applicative notation.
--
-- 'FormParser' is parametrized by four type variables:
--
--     * @names@—collection of field names we can use in a form to be parsed
--       with this parser.
--     * @e@—type of validation errors.
--     * @m@—underlying monad, 'FormParser' is not a monad itself, so it's
--       not a monad transformer, but validation can make use of the @m@
--       monad.
--     * @a@—result of parsing.
--
-- 'FormParser' is not a monad because it's not possible to write a 'Monad'
-- instance with the properties that we want (validation errors should not
-- lead to short-cutting behavior).

newtype FormParser (names :: [Symbol]) e m a = FormParser
  { unFormParser
      :: Value
      -> Maybe (FieldName names)
      -> m (FormResult names e a)
  }

instance Functor m => Functor (FormParser names e m) where
  fmap f (FormParser x) = FormParser $ \v path ->
    fmap (fmap f) (x v path)

instance Applicative m => Applicative (FormParser names e m) where
  pure x = FormParser $ \_ _ ->
    pure (Succeeded x)
  (FormParser f) <*> (FormParser x) = FormParser $ \v path ->
    pure (<*>) <*> f v path <*> x v path

instance Applicative m => Alternative (FormParser names e m) where
  empty = FormParser $ \_ _ ->
    pure (ParsingFailed Nothing "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v path ->
    let g x' y' =
          case x' of
            ParsingFailed  _ _ -> y'
            ValidationFailed _ -> x'
            Succeeded        _ -> x'
    in pure g <*> x v path <*> y v path

-- | @'FieldName' names@ represents a non-empty vector of 'Text' components
-- that serve as a path to some field in a JSON structure. Every component
-- is guaranteed to be in the @names@, which is a set of strings on type
-- level. The purpose if this type is to avoid typos and to force users to
-- update field names everywhere when they decide to change them. The only
-- way to obtain a value of the type 'FieldName' is by using
-- @OverloadedLabels@. Note that you can combine field names using @('<>')@.
--
-- > showFieldName (#login_form <> #username) = "login_form.username"

newtype FieldName (names :: [Symbol])
  = FieldName (NonEmpty Text)
  deriving (Eq, Ord, Show)

instance (KnownSymbol name, InSet name names)
  => IsLabel (name :: Symbol) (FieldName names) where
  fromLabel =
    (FieldName . nes . T.pack . symbolVal) (Proxy :: Proxy name)
    where
      nes x = x :| []

instance Semigroup (FieldName names) where
  FieldName x <> FieldName y = FieldName (x <> y)

instance ToJSON (FieldName names) where
  toJSON = toJSON . showFieldName

-- | Project field path from a 'FieldName'.

unFieldName :: FieldName names -> NonEmpty Text
unFieldName (FieldName path) = path

-- | Project textual representation of path to a field.

showFieldName :: FieldName names -> Text
showFieldName = T.intercalate "." . NE.toList . unFieldName

-- | The type function computes a 'Constraint' which is satisfied when its
-- first argument is contained in its second argument. Otherwise a friendly
-- type error is displayed.

type family InSet (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  InSet n '[]    = TypeError
    ('Text "The name " ':<>: 'ShowType n ':<>: 'Text " is not in the given set."
     ':$$:
     'Text "Either it's a typo or you need to add it to the set first.")
  InSet n (n:ns) = ()
  InSet n (m:ns) = InSet n ns
