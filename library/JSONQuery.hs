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
  JSON -> Either Text a

run :: Q a -> JSON -> Either Text a
run = id

value :: Text -> Q JSON
value name =
  \case
    Aeson.Object m -> 
      maybe (Left $ "Object contains no field '" <> name <> "'") Right $
      HashMap.lookup name m
    _ ->
      Left "Not an object"

element :: Int -> Q JSON
element index =
  \case
    Aeson.Array v ->
      maybe (Left $ "Array has no index '" <> (fromString . show) index <> "'") Right $
      v Vector.!? index
    _ ->
      Left "Not an array"

string :: Q Text
string =
  \case
    Aeson.String t ->
      Right t
    _ ->
      Left "Not a string"

number :: Q Scientific
number =
  \case
    Aeson.Number x ->
      Right x
    _ ->
      Left "Not a number"

bool :: Q Bool
bool =
  \case
    Aeson.Bool x -> 
      Right x
    _ -> 
      Left "Not a bool"

nullable :: Q a -> Q (Maybe a)
nullable q =
  \case
    Aeson.Null ->
      Right Nothing
    x -> 
      fmap Just $ run q x

arrayOf :: Q a -> Q (Vector.Vector a)
arrayOf q =
  \case
    Aeson.Array v ->
      Vector.mapM (run q) v
    _ ->
      Left "Not an array"

objectOf :: Q a -> Q (HashMap.HashMap Text a)
objectOf q =
  \case
    Aeson.Object m ->
      mapM (run q) m
    _ ->
      Left "Not an object"

fromJSON :: Aeson.FromJSON a => Q a
fromJSON =
  Aeson.fromJSON >>> \case
    Aeson.Error m -> Left $ fromString m
    Aeson.Success r -> Right $ r

