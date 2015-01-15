module JSONQuery where

import BasePrelude hiding (bool)
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector


type JSON = 
  Aeson.Value

type Q a =
  JSON -> Result a

newtype Result a =
  Result { resultEither :: Either Text a }
  deriving (Functor, Applicative, Monad)

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

value :: Text -> Q JSON
value name =
  \case
    Aeson.Object m -> 
      maybe (Result $ Left $ "Object contains no field '" <> name <> "'") return $
      HashMap.lookup name m
    _ ->
      Result $ Left "Not an object"

element :: Int -> Q JSON
element index =
  \case
    Aeson.Array v ->
      maybe (Result $ Left $ "Array has no index '" <> (fromString . show) index <> "'") return $
      v Vector.!? index
    _ ->
      Result $ Left "Not an array"

string :: Q Text
string =
  \case
    Aeson.String t ->
      return t
    _ ->
      Result $ Left "Not a string"

number :: Q Scientific
number =
  \case
    Aeson.Number x ->
      return x
    _ ->
      Result $ Left "Not a number"

bool :: Q Bool
bool =
  \case
    Aeson.Bool x -> 
      return x
    _ -> 
      Result $ Left "Not a bool"

nullable :: Q a -> Q (Maybe a)
nullable q =
  \case
    Aeson.Null ->
      return Nothing
    x -> 
      fmap Just $ q x

arrayOf :: Q a -> Q (Vector.Vector a)
arrayOf q =
  \case
    Aeson.Array v ->
      Vector.mapM q v
    _ ->
      Result $ Left "Not an array"

objectOf :: Q a -> Q (HashMap.HashMap Text a)
objectOf q =
  \case
    Aeson.Object m ->
      mapM q m
    _ ->
      Result $ Left "Not an object"

fromJSON :: Aeson.FromJSON a => Q a
fromJSON =
  Aeson.fromJSON >>> \case
    Aeson.Error m -> Result $ Left $ fromString m
    Aeson.Success r -> return $ r
