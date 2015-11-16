module Aeson.ValueParser
(
  ValueParser,
  ArrayParser,
  ObjectParser,
  run,
  -- * Value parsers
  array,
  object,
  nullable,
  string,
  number,
  bool,
  fromJSON,
  -- * Object parsers
  field,
  allFields,
  -- * Array parsers
  element,
  allElements,
)
where

import BasePrelude hiding (bool)
import MTLPrelude
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as B
import qualified Data.Vector as C


newtype Result a =
  Result { resultEither :: Either Text a }
  deriving (Functor, Applicative, Monad, MonadError Text)

instance Alternative Result where
  empty = 
    Result $ Left "No result"
  (<|>) =
    \case
      Result (Left _) -> id
      r -> const r

instance MonadPlus Result where
  mzero = empty
  mplus = (<|>)


type ValueParser =
  ReaderT A.Value Result

type ArrayParser =
  ReaderT A.Array Result

type ObjectParser =
  ReaderT A.Object Result

run :: ValueParser a -> A.Value -> Either Text a
run effect =
  resultEither . runReaderT effect

-- * Value parsers
-------------------------

array :: ArrayParser a -> ValueParser a
array effect =
  ReaderT $ \case
    A.Array x ->
      runReaderT effect x
    _ ->
      Result $ Left "Not an array"

object :: ObjectParser a -> ValueParser a
object effect =
  ReaderT $ \case
    A.Object x ->
      runReaderT effect x
    _ ->
      Result $ Left "Not an object"

nullable :: ValueParser a -> ValueParser (Maybe a)
nullable q =
  ReaderT $ \case
    A.Null ->
      return Nothing
    x -> 
      Result $ fmap Just $ run q x

string :: ValueParser Text
string =
  ReaderT $ \case
    A.String t ->
      return t
    _ ->
      Result $ Left "Not a string"

number :: ValueParser Scientific
number =
  ReaderT $ \case
    A.Number x ->
      return x
    _ ->
      Result $ Left "Not a number"

bool :: ValueParser Bool
bool =
  ReaderT $ \case
    A.Bool x -> 
      return x
    _ -> 
      Result $ Left "Not a bool"

fromJSON :: A.FromJSON a => ValueParser a
fromJSON =
  ReaderT $ A.fromJSON >>> \case
    A.Error m -> Result $ Left $ fromString m
    A.Success r -> Result $ Right $ r

-- * Object parsers
-------------------------

field :: Text -> ValueParser a -> ObjectParser a
field key effect =
  ReaderT $
    maybe (Result $ Left $ "Object contains no field '" <> key <> "'") (runReaderT effect) .
    B.lookup key

allFields :: ValueParser a -> ObjectParser (B.HashMap Text a)
allFields effect =
  ReaderT $ mapM (runReaderT effect)

-- * Array parsers
-------------------------

element :: Int -> ValueParser a -> ArrayParser a
element element effect =
  ReaderT $ 
    maybe (Result $ Left $ "Array has no element '" <> (fromString . show) element <> "'") (runReaderT effect) .
    flip (C.!?) element

allElements :: ValueParser a -> ArrayParser (C.Vector a)
allElements effect =
  ReaderT $ mapM (runReaderT effect)
