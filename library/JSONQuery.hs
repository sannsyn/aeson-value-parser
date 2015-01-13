module JSONQuery where

import BasePrelude hiding (maybe, bool)
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector


type JSON = 
  Aeson.Value

type Q =
  Kleisli [] JSON

run :: Q a -> JSON -> [a]
run = 
  runKleisli

value :: Text -> Q JSON
value name =
  Kleisli $ \case
    Aeson.Object m -> toList $ HashMap.lookup name m
    _ -> []

values :: Q JSON
values =
  Kleisli $ \case
    Aeson.Object m -> HashMap.elems m
    _ -> []

element :: Int -> Q JSON
element index =
  Kleisli $ \case
    Aeson.Array v -> toList $ v Vector.!? index
    _ -> []

elements :: Q JSON
elements =
  Kleisli $ \case
    Aeson.Array v -> toList v
    _ -> []

