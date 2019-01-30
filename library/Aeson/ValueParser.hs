-- |
-- A parser DSL for the \"aeson\" model of the JSON tree.
module Aeson.ValueParser
(
  Value,
  run,
  Error.Error(..),
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
  -- * Object parsers
  Object,
  field,
  oneOfFields,
  fieldMap,
  foldlFields,
  -- * Array parsers
  Array,
  element,
  elementVector,
  foldlElements,
  foldrElements,
)
where

import Aeson.ValueParser.Prelude hiding (bool, null)
import qualified Aeson.ValueParser.Error as Error
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Aeson.ValueParser.Vector as Vector


-- * Shared
-------------------------

type Parse ast = ReaderT ast (ExceptT Error.Error (Except Error.Error))


-- * Value
-------------------------

-- |
-- A JSON 'Aeson.Value' parser.
newtype Value a =
  Value (Parse Aeson.Value a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Value where
  fail = throwError . fromString

{-# INLINE run #-}
run :: Value a -> Aeson.Value -> Either Error.Error a
run (Value effect) = join . runExcept . runExceptT . runReaderT effect

-- ** Definitions
-------------------------

{-# INLINE astMatcher #-}
astMatcher :: (Aeson.Value -> Either Text a) -> Value a
astMatcher matcher = Value $ ReaderT $ either (throwE . Error.message) return . matcher

{-# INLINE array #-}
array :: Array a -> Value a
array (Array parser) = Value $ ReaderT $ \ case
  Aeson.Array x -> runReaderT parser x
  Aeson.Object _ -> throwError "Unexpected type: object"
  Aeson.String _ -> throwError "Unexpected type: string"
  Aeson.Number _ -> throwError "Unexpected type: number"
  Aeson.Bool _ -> throwError "Unexpected type: bool"
  Aeson.Null -> throwError "Unexpected value: null"

{-# INLINE object #-}
object :: Object a -> Value a
object (Object effect) = Value $ ReaderT $ \ case
  Aeson.Object x -> runReaderT effect x
  Aeson.Array _ -> throwError "Unexpected type: array"
  Aeson.String _ -> throwError "Unexpected type: string"
  Aeson.Number _ -> throwError "Unexpected type: number"
  Aeson.Bool _ -> throwError "Unexpected type: bool"
  Aeson.Null -> throwError "Unexpected value: null"

{-# INLINE null #-}
null :: Value ()
null = astMatcher $ \ case
  Aeson.Null -> pure ()
  Aeson.Object _ -> throwError "Unexpected type: object"
  Aeson.Array _ -> throwError "Unexpected type: array"
  Aeson.String _ -> throwError "Unexpected type: string"
  Aeson.Number _ -> throwError "Unexpected type: number"
  Aeson.Bool _ -> throwError "Unexpected type: bool"

{-# INLINE nullable #-}
nullable :: Value a -> Value (Maybe a)
nullable (Value parser) = Value $ ReaderT $ \ case
  Aeson.Null -> pure Nothing
  x -> fmap Just (runReaderT parser x)

{-# INLINE string #-}
string :: Value Text
string = astMatcher $ \ case
  Aeson.String t -> pure t
  Aeson.Object _ -> throwError "Unexpected type: object"
  Aeson.Array _ -> throwError "Unexpected type: array"
  Aeson.Number _ -> throwError "Unexpected type: number"
  Aeson.Bool _ -> throwError "Unexpected type: bool"
  Aeson.Null -> throwError "Unexpected value: null"

{-# INLINE stringAsBytes #-}
stringAsBytes :: Value ByteString
stringAsBytes = Text.encodeUtf8 <$> string

{-# INLINE number #-}
number :: Value Scientific
number = astMatcher $ \ case
  Aeson.Number x -> pure x
  Aeson.Object _ -> throwError "Unexpected type: object"
  Aeson.Array _ -> throwError "Unexpected type: array"
  Aeson.String _ -> throwError "Unexpected type: string"
  Aeson.Bool _ -> throwError "Unexpected type: bool"
  Aeson.Null -> throwError "Unexpected value: null"

{-# INLINE numberAsInt #-}
numberAsInt :: Value Int
numberAsInt = do
  x <- number
  if Scientific.isInteger x
    then case Scientific.toBoundedInteger x of
      Just int -> return int
      Nothing -> fail ("Number " <> show x <> " is out of integer range")
    else fail ("Number " <> show x <> " is not an integer")

{-# INLINE bool #-}
bool :: Value Bool
bool = astMatcher $ \ case
  Aeson.Bool x -> pure x
  Aeson.Object _ -> throwError "Unexpected type: object"
  Aeson.Array _ -> throwError "Unexpected type: array"
  Aeson.String _ -> throwError "Unexpected type: string"
  Aeson.Number _ -> throwError "Unexpected type: number"
  Aeson.Null -> throwError "Unexpected value: null"

{-# INLINE fromJSON #-}
fromJSON :: Aeson.FromJSON a => Value a
fromJSON = Value $ ReaderT $ Aeson.fromJSON >>> \ case
  Aeson.Success r -> return r
  Aeson.Error m -> lift $ throwE $ fromString m


-- * Object parsers
-------------------------

-- |
-- A JSON 'Aeson.Object' parser.
newtype Object a =
  Object (Parse (HashMap Text Aeson.Value) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Object where
  fail = throwError . fromString

{-# INLINE field #-}
field :: Text -> Value a -> Object a
field name fieldParser = Object $ ReaderT $ \ object -> case HashMap.lookup name object of
  Just value -> case run fieldParser value of
    Right parsedValue -> return parsedValue
    Left error -> lift $ throwE $ Error.named name error
  Nothing -> throwE (Error.Error (pure name) "Object contains no field with this name")

{-# INLINE oneOfFields #-}
oneOfFields :: [Text] -> Value a -> Object a
oneOfFields keys valueParser = asum (fmap (flip field valueParser) keys)

{-# INLINE fieldMap #-}
fieldMap :: Value a -> Object (HashMap Text a)
fieldMap fieldParser = Object $ ReaderT $ HashMap.traverseWithKey mapping where
  mapping key ast = case run fieldParser ast of
    Right parsedField -> return parsedField
    Left error -> lift (throwE (Error.named key error))

{-# INLINE foldlFields #-}
foldlFields :: (state -> Text -> field -> state) -> state -> Value field -> Object state
foldlFields step state fieldParser = Object $ ReaderT $ \ object -> HashMap.foldlWithKey' newStep (pure state) object where
  newStep stateE key fieldAst = case run fieldParser fieldAst of
    Right !parsedField -> do
      !state <- stateE
      return $ step state key parsedField
    Left error -> lift (throwE (Error.named key error))


-- * Array parsers
-------------------------

-- |
-- A JSON 'Aeson.Array' parser.
newtype Array a =
  Array (Parse (Vector Aeson.Value) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Array where
  fail = throwError . fromString

{-# INLINE element #-}
element :: Int -> Value a -> Array a
element index elementParser = Array $ ReaderT $ \ array -> case array Vector.!? index of
  Just element -> case run elementParser element of
    Right result -> return result
    Left error -> lift $ throwE $ Error.indexed index error
  Nothing -> throwE $ Error.Error (pure (fromString (show index))) "Array contains no element by this index"

{-# INLINE elementVector #-}
elementVector :: Value a -> Array (Vector a)
elementVector elementParser = Array $ ReaderT $ \ arrayAst -> flip Vector.imapM arrayAst $ \ index ast -> case run elementParser ast of
  Right element -> return element
  Left error -> lift $ throwE $ Error.indexed index error

{-# INLINE foldlElements #-}
foldlElements :: (state -> Int -> element -> state) -> state -> Value element -> Array state
foldlElements step state elementParser = Array $ ReaderT $ Vector.ifoldM' newStep state where
  newStep state index ast = case run elementParser ast of
    Right element -> return $ step state index element
    Left error -> lift $ throwE $ Error.indexed index error

{-# INLINE foldrElements #-}
foldrElements :: (Int -> element -> state -> state) -> state -> Value element -> Array state
foldrElements step state elementParser = Array $ ReaderT $ Vector.ifoldrM newStep state where
  newStep index ast nextState = case run elementParser ast of
    Right element -> return $ step index element nextState
    Left error -> lift $ throwE $ Error.indexed index error
