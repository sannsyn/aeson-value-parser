{-
Parser DSL for the \"aeson\" model of JSON tree.

The general model of this DSL is about switching between contexts.
-}
module Aeson.ValueParser
(
  Value,
  run,
  runWithTextError,
  Error.Error(..),
  -- * Value parsers
  object,
  array,
  null,
  nullable,
  nullableMonoid,
  string,
  number,
  bool,
  fromJSON,
  -- * String parsers
  String,
  text,
  matchedText,
  parsedText,
  -- * Number parsers
  Number,
  scientific,
  integer,
  floating,
  matchedScientific,
  matchedInteger,
  matchedFloating,
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

import Aeson.ValueParser.Prelude hiding (bool, null, String)
import qualified Aeson.ValueParser.Error as Error
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Aeson.ValueParser.Vector as Vector


-- * Value
-------------------------

{-|
JSON `Aeson.Value` AST parser.

Its `Alternative` instance implements the logic of choosing between the possible types of JSON values.
-}
newtype Value a =
  Value (ReaderT Aeson.Value (MaybeT (Either Error.Error)) a)
  deriving (Functor, Applicative)

{-|
Implements the logic of choosing between the possible types of JSON values.

If you have multiple parsers of the same type of JSON value composed,
only the leftmost will be affective.
The errors from deeper parsers do not trigger the alternation,
instead they get propagated to the top.
-}
instance Alternative Value where
  empty = Value $ ReaderT $ const $ MaybeT $ return Nothing
  (<|>) (Value leftParser) (Value rightParser) = Value (leftParser <|> rightParser)

{-# INLINE run #-}
run :: Value a -> Aeson.Value -> Either Error.Error a
run = \ (Value parser) value -> (=<<) (maybe (Left (typeError value)) Right) $ runMaybeT $ runReaderT parser value where
  typeError = \ case
    Aeson.Array _ -> "Unexpected type: array"
    Aeson.Object _ -> "Unexpected type: object"
    Aeson.String _ -> "Unexpected type: string"
    Aeson.Number _ -> "Unexpected type: number"
    Aeson.Bool _ -> "Unexpected type: bool"
    Aeson.Null -> "Unexpected type: null"

{-# INLINE runWithTextError #-}
runWithTextError :: Value a -> Aeson.Value -> Either Text a
runWithTextError parser = left Error.toText . run parser

runString :: String a -> Text -> Either (Maybe Text) a
runString (String a) b = runExcept (runReaderT a b)

-- ** Definitions
-------------------------

{-# INLINE array #-}
array :: Array a -> Value a
array (Array parser) = Value $ ReaderT $ \ case
  Aeson.Array x -> lift $ join $ runExcept $ runExceptT $ runReaderT parser x
  _ -> empty

{-# INLINE object #-}
object :: Object a -> Value a
object (Object parser) = Value $ ReaderT $ \ case
  Aeson.Object x -> lift $ join $ runExcept $ runExceptT $ runReaderT parser x
  _ -> empty

{-# INLINE null #-}
null :: Value ()
null = Value $ ReaderT $ \ case
  Aeson.Null -> pure ()
  _ -> empty

{-# INLINE nullable #-}
nullable :: Value a -> Value (Maybe a)
nullable (Value parser) = Value $ ReaderT $ \ case
  Aeson.Null -> pure Nothing
  x -> fmap Just (runReaderT parser x)

{-# INLINE nullableMonoid #-}
nullableMonoid :: Monoid a => Value a -> Value a
nullableMonoid (Value parser) = Value $ ReaderT $ \ case
  Aeson.Null -> pure mempty
  x -> runReaderT parser x

{-# INLINE string #-}
string :: String a -> Value a
string (String parser) = Value $ ReaderT $ \ case
  Aeson.String x -> lift $ left (Error.message . fromMaybe "No details") $ runExcept $ runReaderT parser x
  _ -> empty

{-# INLINE number #-}
number :: Number a -> Value a
number (Number parser) = Value $ ReaderT $ \ case
  Aeson.Number x -> lift $ left (Error.message . fromMaybe "No details") $ runExcept $ runReaderT parser x
  _ -> empty

{-# INLINE bool #-}
bool :: Value Bool
bool = Value $ ReaderT $ \ case
  Aeson.Bool x -> return x
  _ -> empty

{-# INLINE fromJSON #-}
fromJSON :: Aeson.FromJSON a => Value a
fromJSON = Value $ ReaderT $ Aeson.fromJSON >>> \ case
  Aeson.Success r -> return r
  Aeson.Error m -> lift $ Left $ fromString m


-- * String parsers
-------------------------

newtype String a =
  String (ReaderT Text (Except (Maybe Text)) a)
  deriving (Functor, Applicative, Alternative)

{-# INLINE text #-}
text :: String Text
text = String ask

{-# INLINE matchedText #-}
matchedText :: (Text -> Either Text a) -> String a
matchedText parser = String $ ReaderT $ except . left Just . parser

{-# INLINE parsedText #-}
parsedText :: Attoparsec.Parser a -> String a
parsedText parser = matchedText $ left fromString . Attoparsec.parseOnly parser


-- * Number parsers
-------------------------

newtype Number a =
  Number (ReaderT Scientific (Except (Maybe Text)) a)
  deriving (Functor, Applicative, Alternative)

{-# INLINE scientific #-}
scientific :: Number Scientific
scientific = Number ask

{-# INLINE integer #-}
integer :: (Integral a, Bounded a) => Number a
integer = Number $ ReaderT $ \ x -> if Scientific.isInteger x
  then case Scientific.toBoundedInteger x of
    Just int -> return int
    Nothing -> throwError (Just (fromString ("Number " <> show x <> " is out of integer range")))
  else throwError (Just (fromString ("Number " <> show x <> " is not integer")))

{-# INLINE floating #-}
floating :: RealFloat a => Number a
floating = Number $ ReaderT $ \ a -> case Scientific.toBoundedRealFloat a of
  Right b -> return b
  Left c -> if c == 0
    then throwError (Just (fromString ("Number " <> show a <> " is too small")))
    else throwError (Just (fromString ("Number " <> show a <> " is too large")))

{-# INLINE matchedScientific #-}
matchedScientific :: (Scientific -> Either Text a) -> Number a
matchedScientific matcher = Number $ ReaderT $ except . left Just . matcher

{-# INLINE matchedInteger #-}
matchedInteger :: (Integral integer, Bounded integer) => (integer -> Either Text a) -> Number a
matchedInteger matcher = Number $ case integer of
  Number parser -> parser >>= either (throwError . Just) return . matcher

{-# INLINE matchedFloating #-}
matchedFloating :: RealFloat floating => (floating -> Either Text a) -> Number a
matchedFloating matcher = Number $ case floating of
  Number parser -> parser >>= either (throwError . Just) return . matcher


-- * Object parsers
-------------------------

{-|
JSON `Aeson.Object` parser.
-}
newtype Object a =
  Object (ReaderT (HashMap Text Aeson.Value) (ExceptT Error.Error (Except Error.Error)) a)
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
fieldMap :: (Eq a, Hashable a) => String a -> Value b -> Object (HashMap a b)
fieldMap keyParser fieldParser = Object $ ReaderT $ fmap HashMap.fromList . traverse mapping . HashMap.toList where
  mapping (keyText, ast) = 
    case runString keyParser keyText of
      Right parsedKey -> case run fieldParser ast of
        Right parsedField -> return (parsedKey, parsedField)
        Left error -> lift (throwE (Error.named keyText error))
      Left error -> lift (throwE (maybe mempty Error.message error))

{-# INLINE foldlFields #-}
foldlFields :: (state -> key -> field -> state) -> state -> String key -> Value field -> Object state
foldlFields step state keyParser fieldParser = Object $ ReaderT $ \ object -> HashMap.foldlWithKey' newStep (pure state) object where
  newStep stateE key fieldAst = 
    case runString keyParser key of
      Right !parsedKey -> case run fieldParser fieldAst of
        Right !parsedField -> do
          !state <- stateE
          return $ step state parsedKey parsedField
        Left error -> lift (throwE (Error.named key error))
      Left error -> lift (throwE (maybe mempty Error.message error))


-- * Array parsers
-------------------------

{-|
JSON `Aeson.Array` parser.
-}
newtype Array a =
  Array (ReaderT (Vector Aeson.Value) (ExceptT Error.Error (Except Error.Error)) a)
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
