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
  fieldOr,
  oneOfFieldsOr,
  possibleField,
  oneOfPossibleFields,
  matchField,
  fieldMap,
  foldlFields,
  -- * Array parsers
  Array,
  element,
  elementOr,
  possibleElement,
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


-- * Value
-------------------------

-- |
-- A JSON 'Aeson.Value' parser.
newtype Value a =
  Value (ReaderT Aeson.Value (Except Error.Error) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Value where
  fail = throwError . fromString

{-# INLINE run #-}
run :: Value a -> Aeson.Value -> Either Error.Error a
run (Value effect) = runExcept . runReaderT effect

-- ** Definitions
-------------------------

{-# INLINE astMatcher #-}
astMatcher :: (Aeson.Value -> Either Text a) -> Value a
astMatcher matcher = Value $ ReaderT $ except . left Error.message . matcher

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
  Aeson.Error m -> (except . Left) (fromString m)
  Aeson.Success r -> pure r


-- * Object parsers
-------------------------

-- |
-- A JSON 'Aeson.Object' parser.
newtype Object a =
  Object (ReaderT (HashMap Text Aeson.Value) (Except Error.Error) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Object where
  fail = throwError . fromString

{-# INLINE field #-}
field :: Text -> Value a -> Object a
field key (Value effect) = Object $ ReaderT $ \ object -> except $ case HashMap.lookup key object of
  Just value -> case runExcept (runReaderT effect value) of
    Right parsedValue -> Right parsedValue
    Left error -> Left (Error.named key error)
  Nothing -> Left (Error.Error (pure key) "Object contains no such key")

{-# INLINE oneOfFields #-}
oneOfFields :: [Text] -> Value a -> Object a
oneOfFields keys valueParser = asum (fmap (flip field valueParser) keys)

{-# INLINE possibleFieldInChurch #-}
possibleFieldInChurch :: b -> (a -> b) -> Text -> Value a -> Object b
possibleFieldInChurch nothing just key fieldParser = Object $ ReaderT $ \ object -> except $ case HashMap.lookup key object of
  Just value -> case run fieldParser value of
    Right parsedValue -> Right (just parsedValue)
    Left error -> Left (Error.named key error)
  Nothing -> Right nothing

{-| Applies the value parser to the existing field, propagating its errors accordingly.
    Only if the field does not exist, it executes /Alternative branch/.
    IOW, it won't execute /Alternative branch/ if /Field parser/ fails.
    This is what distinguishes @fieldOr name parser@ from the seemingly same @mplus (field name parser)@ -}
fieldOr :: Text {-^ Field name -} -> Value a {-^ Field parser -} -> Object a {-^ Alternative branch -} -> Object a
fieldOr name fieldParser alt = join $ possibleFieldInChurch alt return name fieldParser

{-| Same as `fieldOr`,
    with the only difference that it enumerates the provided list of field names
    parsing the first existing one. -}
oneOfFieldsOr :: [Text] -> Value a -> Object a -> Object a
oneOfFieldsOr names valueParser objectParser = foldr (\ name -> fieldOr name valueParser) objectParser names

{-| Applies the value parser to the existing field, propagating its errors accordingly, but
    unlike `field` it doesn't fail if the field does not exist.
    The `Maybe` wrapper is there to cover such a case. -}
{-# INLINE possibleField #-}
possibleField :: Text -> Value a -> Object (Maybe a)
possibleField = possibleFieldInChurch Nothing Just

{-| Same as `possibleField`,
    with the only difference that it enumerates the provided list of field names
    parsing the first existing one. -}
{-# INLINE oneOfPossibleFields #-}
oneOfPossibleFields :: [Text] -> Value a -> Object (Maybe a)
oneOfPossibleFields keys valueParser = oneOfFieldsOr keys (fmap Just valueParser) (pure Nothing)

{-| Branch out the parsing scenario based on which fields are defined.
    The value parser returns a continuation of the object parser,
    thus allowing you to continue parsing the object
    in the context of a parsed field.
    
    Fails if the list of provided cases is empty. -}
{-# INLINE matchField #-}
matchField :: [(Text, Value (Object a))] -> Object a
matchField = \ cases -> foldr tryCaseOr (noneFound (HashSet.fromList (fmap fst cases))) cases where
  tryCaseOr (name, fieldParser) alt = join $ possibleFieldInChurch alt id name fieldParser
  noneFound nameSet = fail ("None of the following fields were found: " <> show (toList nameSet))

{-# INLINE fieldMap #-}
fieldMap :: Value a -> Object (HashMap Text a)
fieldMap fieldParser = Object $ ReaderT $ except . HashMap.traverseWithKey mapping where
  mapping key ast = case run fieldParser ast of
    Right parsedField -> return parsedField
    Left error -> Left (Error.named key error)

{-# INLINE foldlFields #-}
foldlFields :: (state -> Text -> field -> state) -> state -> Value field -> Object state
foldlFields step state fieldParser = Object $ ReaderT $ \ object -> except $ HashMap.foldlWithKey' newStep (pure state) object where
  newStep stateEither key fieldAst = case run fieldParser fieldAst of
    Right !parsedField -> do
      !state <- stateEither
      return $ step state key parsedField
    Left error -> Left (Error.named key error)


-- * Array parsers
-------------------------

-- |
-- A JSON 'Aeson.Array' parser.
newtype Array a =
  Array (ReaderT (Vector Aeson.Value) (Except Error.Error) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Error.Error)

instance MonadFail Array where
  fail = throwError . fromString

{-# INLINE element #-}
element :: Int -> Value a -> Array a
element index elementParser = Array $ ReaderT $ \ array -> except $ case array Vector.!? index of
  Just element -> case run elementParser element of
    Right result -> Right result
    Left error -> Left (Error.indexed index error)
  Nothing -> Left (Error.Error (pure (fromString (show index))) "Array contains no element by this index")

{-# INLINE elementOr #-}
elementOr :: Int -> Value a -> Array a -> Array a
elementOr index elementParser alt = join $ Array $ ReaderT $ \ array -> except $ case array Vector.!? index of
  Just element -> case run elementParser element of
    Right result -> Right (return result)
    Left error -> Left (Error.indexed index error)
  Nothing -> Right alt

{-# INLINE possibleElement #-}
possibleElement :: Int -> Value a -> Array (Maybe a)
possibleElement index elementParser = Array $ ReaderT $ \ array -> except $ case array Vector.!? index of
  Just element -> case run elementParser element of
    Right result -> Right (Just result)
    Left error -> Left (Error.indexed index error)
  Nothing -> Right Nothing

{-# INLINE elementVector #-}
elementVector :: Value a -> Array (Vector a)
elementVector elementParser = Array $ ReaderT $ \ arrayAst -> except $ flip Vector.imapM arrayAst $ \ index ast -> case run elementParser ast of
  Right element -> Right element
  Left error -> Left (Error.indexed index error)

{-# INLINE foldlElements #-}
foldlElements :: (state -> Int -> element -> state) -> state -> Value element -> Array state
foldlElements step state elementParser = Array $ ReaderT $ except . Vector.ifoldM' newStep state where
  newStep state index ast = case run elementParser ast of
    Right !element -> Right $ step state index element
    Left error -> Left (Error.indexed index error)

{-# INLINE foldrElements #-}
foldrElements :: (Int -> element -> state -> state) -> state -> Value element -> Array state
foldrElements step state elementParser = Array $ ReaderT $ except . Vector.ifoldrM newStep state where
  newStep index ast nextState = case run elementParser ast of
    Right !element -> Right $ step index element nextState
    Left error -> Left (Error.indexed index error)
