module Aeson.Query where

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

onArray :: ArrayParser a -> ValueParser a
onArray effect =
  ReaderT $ \case
    A.Array x ->
      runReaderT effect x
    _ ->
      Result $ Left "Not an array"

onNullable :: ValueParser a -> ValueParser (Maybe a)
onNullable q =
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

value :: A.FromJSON a => ValueParser a
value =
  ReaderT $ A.fromJSON >>> \case
    A.Error m -> Result $ Left $ fromString m
    A.Success r -> Result $ Right $ r

-- * Object parsers
-------------------------

onKey :: Text -> ValueParser a -> ObjectParser a
onKey key effect =
  ReaderT $
    maybe (Result $ Left $ "Object contains no field '" <> key <> "'") (runReaderT effect) .
    B.lookup key

onAllKeys :: ValueParser a -> ObjectParser (B.HashMap Text a)
onAllKeys effect =
  ReaderT $ mapM (runReaderT effect)

-- * Array parsers
-------------------------

onIndex :: Int -> ValueParser a -> ArrayParser a
onIndex index effect =
  ReaderT $ 
    maybe (Result $ Left $ "Array has no index '" <> (fromString . show) index <> "'") (runReaderT effect) .
    flip (C.!?) index

onAllIndexes :: ValueParser a -> ArrayParser (C.Vector a)
onAllIndexes effect =
  ReaderT $ mapM (runReaderT effect)
