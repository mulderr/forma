-- |
-- Module      :  Web.Forma
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a tool for validation of forms that are represented
-- in the JSON format. Sending forms in JSON format via an AJAX request
-- instead of traditional submitting of forms has a number of advantages:
--
--     * Smoother user experience: no need to reload the whole page.
--     * Form rendering is separated and lives only in GET handler, POST (or
--       whatever method you deem appropriate for your use case) handler
--       only handles validation and actual effects that form submission
--       should initiate.
--     * You get a chance to organize form input just like you want.
--
-- The task of validation of a form in the JSON format may seem simple, but
-- it's not trivial to get it right. The library allows you to:
--
--     * Define form parser using type-safe applicative notation with field
--       labels being stored on the type label which guards against typos
--       and will force all your field labels be always up to date.
--     * Parse JSON 'Value' according to the definition of form you created.
--     * Stop parsing immediately if given form is malformed and cannot be
--       processed.
--     * Validate forms using any number of /composable/ checkers that you
--       write for your specific problem domain. Once you have a vocabulary
--       of checkers, creation of new forms is just a matter of combining
--       them, and yes they do combine nicely.
--     * Collect validation errors from multiple branches of parsing (one
--       branch per form field) in parallel, so validation errors in one
--       branch do not prevent us from collecting validation errors from
--       other branches. This allows for a better user experience as the
--       user can see all validation errors at the same time.
--     * Use 'optional' and @('<|>')@ from "Control.Applicative" in your
--       form definitions instead of ugly ad-hoc stuff.
--     * Perform validation using several form fields at once. You choose
--       which “sub-region” of your form a given check will have access to,
--       see 'withCheck'.
--
-- You need to enable at least @DataKinds@ and @OverloadedLabels@ language
-- extensions to use this library.
--
-- __Note__: version /1.0.0/ is completely different from older versions.

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Forma
  ( -- * Constructing a form
    field
  , field'
  , value
  , subParser
  , withCheck
    -- * Running a form\/inspecting result
  , runForm
  , runFormPure
  , unFieldName
  , showFieldName
    -- * Types and type functions
  , FormParser
  , FormResult (..)
  , FieldName
  , InSet
  )
where

import Control.Monad.Except
import Data.Aeson
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.TypeLits
import Web.Forma.Internal
import qualified Data.Aeson.Types    as A
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T

----------------------------------------------------------------------------
-- Constructing a form

-- | Construct a parser for a field. Combine multiple 'field's using
-- applicative syntax like so:
--
-- > type LoginFields = '["username", "password", "remember_me"]
-- >
-- > data LoginForm = LoginForm
-- >   { loginUsername   :: Text
-- >   , loginPassword   :: Text
-- >   , loginRememberMe :: Bool
-- >   }
-- >
-- > loginForm :: Monad m => FormParser LoginFields Text m LoginForm
-- > loginForm = LoginForm
-- >   <$> field #username notEmpty
-- >   <*> field #password notEmpty
-- >   <*> field' #remember_me
-- >
-- > notEmpty :: Monad m => Text -> ExceptT Text m Text
-- > notEmpty txt =
-- >   if T.null txt
-- >     then throwError "This field cannot be empty"
-- >     else return txt
--
-- Referring to the types in the function's signature, @s@ is extracted from
-- JSON 'Value' for you automatically using its 'FromJSON' instance. The
-- field value is taken in assumption that top level 'Value' is a
-- dictionary, and field name is a key in that dictionary. So for example a
-- valid JSON input for the form shown above could be this:
--
-- > {
-- >   "username": "Bob",
-- >   "password": "123",
-- >   "remember_me": true
-- > }
--
-- Once value of type @s@ is extracted, validation phase beings. The
-- supplied checker (you can easily compose them with @('>=>')@, as they are
-- Kleisli arrows) is applied to the @s@ value and validation either
-- succeeds producing an @a@ value, or we collect an error as a value of @e@
-- type.
--
-- To run a form composed from 'field's, see 'runForm'.
--
-- > field fieldName check = withCheck fieldName check (field' fieldName)

field :: forall (names :: [Symbol]) e m a s. (Monad m, FromJSON s)
  => FieldName names
     -- ^ Name of the field
  -> (s -> ExceptT e m a)
     -- ^ Checker that performs validation and possibly transformation of
     -- the field value
  -> FormParser names e m a
field fieldName check = withCheck fieldName check (field' fieldName)

-- | The same as 'field', but does not require a checker.
--
-- > field' fieldName = subParser fieldName value

field' :: forall (names :: [Symbol]) e m a. (Monad m, FromJSON a)
  => FieldName names
     -- ^ Name of the field
  -> FormParser names e m a
field' fieldName = subParser fieldName value

-- | Interpret the current field as a value of type @a@.

value :: (Monad m , FromJSON a) => FormParser names e m a
value = FormParser $ \v path ->
  case A.parseEither parseJSON v of
    Left msg -> return $
      ParsingFailed path (fixupAesonError msg)
    Right x -> return (Succeeded x)

-- | Use a given parser to parse a field. Suppose that you have a parser
-- @loginForm@ that parses a structure like this one:
--
-- > {
-- >   "username": "Bob",
-- >   "password": "123",
-- >   "remember_me": true
-- > }
--
-- Then @subParser #login loginForm@ will parse this:
--
-- > {
-- >   "login": {
-- >      "username": "Bob",
-- >      "password": "123",
-- >      "remember_me": true
-- >    }
-- > }

subParser :: forall (names :: [Symbol]) e m a.
  Monad m
  => FieldName names        -- ^ Field name to descend to
  -> FormParser names e m a -- ^ Subparser
  -> FormParser names e m a -- ^ Wrapped parser
subParser fieldName p = FormParser $ \v path -> do
  let f = withObject "form field" (.: showFieldName fieldName)
      path' = path <> Just fieldName
  case A.parseEither f v of
    Left msg -> do
      let msg' = fixupAesonError msg
      return (ParsingFailed path' msg')
    Right v' ->
      unFormParser p v' path'

-- | Transform a form by applying a checker on its result.
--
-- > passwordsMatch (a, b) = do
-- >   if a == b
-- >     then return a
-- >     else throwError "Passwords don't match!"
-- >
-- > passwordForm =
-- >   withCheck #password_confirmation passwordsMatch
-- >     ((,) <$> field #password notEmpty
-- >          <*> field #password_confirmation notEmpty)
--
-- Note that you must specify the field name on which to add a validation
-- error message in case the check fails. The field name should be relative
-- and point to a field in the argument parser, not full path from top-level
-- of the form. For example this form:
--
-- > biggerForm = subParser #password_form passwordForm
--
-- will report validation error for the field
-- @\"password_form.password_confirmation\"@ if the check fails (note that
-- @\"password_form\"@ is correctly prepended to the field path).

withCheck :: forall (names :: [Symbol]) e m a s. Monad m
  => FieldName names        -- ^ Field to assign validation error to
  -> (s -> ExceptT e m a)   -- ^ The check to perform
  -> FormParser names e m s -- ^ Original parser
  -> FormParser names e m a -- ^ Parser with the check attached
withCheck fieldName check (FormParser f) = FormParser $ \v path -> do
  r <- f v path
  case r of
    Succeeded x -> do
      res <- runExceptT (check x)
      return $ case res of
        Left verr -> do
          let path' = path <> Just fieldName
          ValidationFailed (M.singleton (fromMaybe fieldName path') verr)
        Right y ->
          Succeeded y
    ValidationFailed e ->
      return (ValidationFailed e)
    ParsingFailed path' msg ->
      return (ParsingFailed path' msg)

----------------------------------------------------------------------------
-- Running a form

-- | Run a parser on given input.

runForm :: Monad m
  => FormParser names e m a
     -- ^ The form parser to run
  -> Value
     -- ^ Input for the parser
  -> m (FormResult names e a)
     -- ^ The result of parsing
runForm (FormParser p) v = p v Nothing

-- | Run form purely.
--
-- @since 1.1.0

runFormPure
  :: FormParser names e Identity a
     -- ^ The form parser to run
  -> Value
     -- ^ Input for the parser
  -> FormResult names e a
     -- ^ The result of parsing
runFormPure p v = runIdentity (runForm p v)

----------------------------------------------------------------------------
-- Helpers

-- | Fixup an error message returned by Aeson.

fixupAesonError :: String -> Text
fixupAesonError msg = T.pack (drop 2 (dropWhile (/= ':') msg))
