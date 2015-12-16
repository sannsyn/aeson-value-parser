-- |
-- A parser DSL for the \"aeson\" model of the JSON tree.
module Aeson.ValueParser
(
  Value,
  run,
  -- * Value parsers
  object,
  array,
  nullable,
  string,
  number,
  bool,
  fromJSON,
  -- * Object parsers
  Object,
  field,
  fieldsMap,
  foldlFields,
  -- * Array parsers
  Array,
  element,
  elementsVector,
  foldlElements,
  foldlElements1,
  foldrElements,
)
where

import BasePrelude hiding (bool)
import MTLPrelude
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as B
import qualified Data.Vector as C
import qualified Success.Pure as D


-- |
-- A JSON 'A.Value' parser.
newtype Value a =
  Value (ReaderT A.Value (D.Success Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE run #-}
run :: Value a -> A.Value -> Either (Maybe Text) a
run (Value effect) =
  D.asEither . runReaderT effect

-- -- * Value parsers
-- -------------------------

{-# INLINE array #-}
array :: Array a -> Value a
array (Array effect) =
  Value $ ReaderT $ \case
    A.Array x ->
      runReaderT effect x
    _ ->
      D.failure "Not an array"

{-# INLINE object #-}
object :: Object a -> Value a
object (Object effect) =
  Value $ ReaderT $ \case
    A.Object x ->
      runReaderT effect x
    _ ->
      D.failure "Not an object"

{-# INLINE null #-}
null :: Value ()
null =
  Value $ ReaderT $ \case
    A.Null ->
      pure ()
    _ ->
      D.failure "Not null"

{-# INLINE nullable #-}
nullable :: Value a -> Value (Maybe a)
nullable (Value impl) =
  Value $ ReaderT $ \case
    A.Null ->
      pure Nothing
    x ->
      fmap Just (runReaderT impl x)

{-# INLINE string #-}
string :: Value Text
string =
  Value $ ReaderT $ \case
    A.String t ->
      pure t
    _ ->
      D.failure "Not a string"

{-# INLINE number #-}
number :: Value Scientific
number =
  Value $ ReaderT $ \case
    A.Number x ->
      pure x
    _ ->
      D.failure "Not a number"

{-# INLINE bool #-}
bool :: Value Bool
bool =
  Value $ ReaderT $ \case
    A.Bool x -> 
      pure x
    _ -> 
      D.failure "Not a bool"

{-# INLINE fromJSON #-}
fromJSON :: A.FromJSON a => Value a
fromJSON =
  Value $ ReaderT $ A.fromJSON >>> \case
    A.Error m -> D.failure (fromString m)
    A.Success r -> pure r


-- * Object parsers
-------------------------

-- |
-- A JSON 'A.Object' parser.
newtype Object a =
  Object (ReaderT A.Object (D.Success Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE field #-}
field :: Text -> Value a -> Object a
field key (Value effect) =
  Object $ ReaderT $
    maybe (D.failure $ "Object contains no field '" <> key <> "'") (runReaderT effect) .
    B.lookup key

{-# INLINE fieldsMap #-}
fieldsMap :: Value a -> Object (B.HashMap Text a)
fieldsMap (Value effect) =
  Object $ ReaderT $ mapM (runReaderT effect)

{-# INLINE foldlFields #-}
foldlFields :: (a -> (Text, b) -> a) -> a -> Value b -> Object a
foldlFields step init (Value impl) =
  Object $ ReaderT $ B.foldlWithKey' step' (pure init)
  where
    step' acc' key value =
      acc' >>= \acc -> fmap (step acc . (,) key) (runReaderT impl value)


-- * Array parsers
-------------------------

-- |
-- A JSON 'A.Array' parser.
newtype Array a =
  Array (ReaderT A.Array (D.Success Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

{-# INLINE element #-}
element :: Int -> Value a -> Array a
element element (Value effect) =
  Array $ ReaderT $ 
    maybe (D.failure $ "Array has no element '" <> (fromString . show) element <> "'") (runReaderT effect) .
    flip (C.!?) element

{-# INLINE elementsVector #-}
elementsVector :: Value a -> Array (C.Vector a)
elementsVector (Value effect) =
  Array $ ReaderT $ mapM (runReaderT effect)

{-# INLINE foldlElements #-}
foldlElements :: (a -> b -> a) -> a -> Value b -> Array a
foldlElements step init (Value impl) =
  Array $ ReaderT $ foldlM step' init
  where
    step' acc element =
      fmap (step acc) (runReaderT impl element)

{-# INLINE foldrElements #-}
foldrElements :: (b -> a -> a) -> a -> Value b -> Array a
foldrElements step init (Value impl) =
  Array $ ReaderT $ foldrM step' init
  where
    step' element acc =
      fmap (flip step acc) (runReaderT impl element)

{-# INLINE foldlElements1 #-}
foldlElements1 :: (a -> a -> a) -> Value a -> Array a
foldlElements1 step value =
  foldlElements (\acc input -> maybe (Just input) (Just . flip step input) acc) Nothing value >>= \case
    Nothing -> Array $ lift $ D.failure "Empty array"
    Just x -> pure x
