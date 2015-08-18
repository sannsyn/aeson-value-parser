module Aeson.Query where

import BasePrelude hiding (bool)
import MTLPrelude
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector


newtype Query a =
  Query (ReaderT Aeson.Value Result a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)

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

run :: Query a -> Aeson.Value -> Either Text a
run (Query reader) value =
  runReaderT reader value & \(Result either) -> either

onValue :: Text -> Query a -> Query a
onValue name cont =
  Query $ ReaderT $ \case
    Aeson.Object m -> do
      v <- 
        maybe (Result $ Left $ "Object contains no field '" <> name <> "'") return $
        HashMap.lookup name m
      Result $ run cont v
    _ ->
      Result $ Left "Not an object"

onElement :: Int -> Query a -> Query a
onElement index cont =
  Query $ ReaderT $ \case
    Aeson.Array v -> do
      v <-
        maybe (Result $ Left $ "Array has no index '" <> (fromString . show) index <> "'") return $
        v Vector.!? index
      Result $ run cont v
    _ ->
      Result $ Left "Not an array"

onNullable :: Query a -> Query (Maybe a)
onNullable q =
  Query $ ReaderT $ \case
    Aeson.Null ->
      return Nothing
    x -> 
      Result $ fmap Just $ run q x

onArrayOf :: Query a -> Query (Vector.Vector a)
onArrayOf q =
  Query ask >>= \case
    Aeson.Array v ->
      Query $ lift $ Result $ Vector.mapM (run q) v
    _ ->
      Query $ lift $ Result $ Left "Not an array"

onObjectOf :: Query a -> Query (HashMap.HashMap Text a)
onObjectOf q =
  Query ask >>= Query . lift . Result . \case
    Aeson.Object m ->
      mapM (run q) m
    _ ->
      Left "Not an object"

string :: Query Text
string =
  Query $ ReaderT $ \case
    Aeson.String t ->
      return t
    _ ->
      Result $ Left "Not a string"

number :: Query Scientific
number =
  Query $ ReaderT $ \case
    Aeson.Number x ->
      return x
    _ ->
      Result $ Left "Not a number"

bool :: Query Bool
bool =
  Query $ ReaderT $ \case
    Aeson.Bool x -> 
      return x
    _ -> 
      Result $ Left "Not a bool"

fromJSON :: Aeson.FromJSON a => Query a
fromJSON =
  Query $ ReaderT $ Aeson.fromJSON >>> \case
    Aeson.Error m -> Result $ Left $ fromString m
    Aeson.Success r -> Result $ Right $ r
