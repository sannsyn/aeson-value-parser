-- |
-- A parser DSL for the \"aeson\" model of the JSON tree.
module Aeson.ValueParser
(
  Value,
  run,
  -- * Value parsers
  object,
  array,
  null,
  nullable,
  string,
  stringAsBytes,
  number,
  numberAsInt,
  bool,
  fromJSON,
  pointed,
  -- * Object parsers
  Object,
  field,
  fieldsMap,
  foldFields,
  foldlFields,
  -- * Array parsers
  Array,
  element,
  elementsVector,
  foldElements,
  foldlElements,
  foldlElements1,
  foldrElements,
)
where

import BasePrelude hiding (bool, null)
import MTLPrelude
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.ByteString (ByteString)
import Control.Foldl (Fold(..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as B
import qualified Data.Vector as C
import qualified Data.Text.Encoding as F
import qualified JSONPointer.Model as D
import qualified JSONPointer.Aeson.Interpreter as E


-- |
-- A JSON 'A.Value' parser.
newtype Value a =
  Value (ReaderT A.Value (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

{-# INLINE run #-}
run :: Value a -> A.Value -> Either Text a
run (Value effect) =
  runExcept . runReaderT effect

-- * Value parsers
-------------------------

{-# INLINE aesonMatcher #-}
aesonMatcher :: (A.Value -> Either Text a) -> Value a
aesonMatcher matcher =
  Value $ ReaderT $ either (except . Left) pure . matcher

{-# INLINE array #-}
array :: Array a -> Value a
array (Array effect) =
  Value $ ReaderT $ \case
    A.Array x ->
      runReaderT effect x
    _ ->
      (except . Left) "Not an array"

{-# INLINE object #-}
object :: Object a -> Value a
object (Object effect) =
  Value $ ReaderT $ \case
    A.Object x ->
      runReaderT effect x
    _ ->
      (except . Left) "Not an object"

{-# INLINE null #-}
null :: Value ()
null =
  aesonMatcher $ \case
    A.Null ->
      pure ()
    _ ->
      Left "Not null"

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
  aesonMatcher $ \case
    A.String t ->
      pure t
    _ ->
      Left "Not a string"

{-# INLINE stringAsBytes #-}
stringAsBytes :: Value ByteString
stringAsBytes =
  F.encodeUtf8 <$> string

{-# INLINE number #-}
number :: Value Scientific
number =
  aesonMatcher $ \case
    A.Number x ->
      pure x
    _ ->
      Left "Not a number"

{-# INLINE numberAsInt #-}
numberAsInt :: Value Int
numberAsInt =
  round <$> number

{-# INLINE bool #-}
bool :: Value Bool
bool =
  aesonMatcher $ \case
    A.Bool x -> 
      pure x
    _ -> 
      Left "Not a bool"

{-# INLINE fromJSON #-}
fromJSON :: A.FromJSON a => Value a
fromJSON =
  Value $ ReaderT $ A.fromJSON >>> \case
    A.Error m -> (except . Left) (fromString m)
    A.Success r -> pure r

{-|
Lifts JSON Pointer.
-}
{-# INLINE pointed #-}
pointed :: D.JSONPointer -> Value a -> Value a
pointed pointer parser =
  aesonMatcher $ \value ->
  case E.value pointer value of
    Nothing -> Left (fromString (showString "Pointer \"" $ shows pointer "\" points to nothing"))
    Just pointedValue -> run parser pointedValue


-- * Object parsers
-------------------------

-- |
-- A JSON 'A.Object' parser.
newtype Object a =
  Object (ReaderT A.Object (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

{-# INLINE field #-}
field :: Text -> Value a -> Object a
field key (Value effect) =
  Object $ ReaderT $
    maybe ((except . Left) $ "Object contains no field '" <> key <> "'") (runReaderT effect) .
    B.lookup key

{-# INLINE fieldsMap #-}
fieldsMap :: Value a -> Object (B.HashMap Text a)
fieldsMap (Value effect) =
  Object $ ReaderT $ mapM (runReaderT effect)

{-# INLINE foldFields #-}
foldFields :: Fold (Text, field) object -> Value field -> Object object
foldFields (Fold foldStep foldInit foldEnd) value =
  fmap foldEnd (foldlFields foldStep foldInit value)

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
  Array (ReaderT A.Array (Except Text) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

{-# INLINE element #-}
element :: Int -> Value a -> Array a
element element (Value effect) =
  Array $ ReaderT $ 
    maybe ((except . Left) $ "Array has no element '" <> (fromString . show) element <> "'") (runReaderT effect) .
    flip (C.!?) element

{-# INLINE elementsVector #-}
elementsVector :: Value a -> Array (C.Vector a)
elementsVector (Value effect) =
  Array $ ReaderT $ mapM (runReaderT effect)

{-# INLINE foldElements #-}
foldElements :: Fold element array -> Value element -> Array array
foldElements (Fold foldStep foldInit foldEnd) value =
  fmap foldEnd (foldlElements foldStep foldInit value)

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
    Nothing -> Array $ lift $ (except . Left) "Empty array"
    Just x -> pure x
