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

by :: Text -> Q JSON
by name =
  Kleisli $ \case
    Aeson.Object m -> toList $ HashMap.lookup name m
    _ -> []

at :: Int -> Q JSON
at index =
  Kleisli $ \case
    Aeson.Array v -> toList $ v Vector.!? index
    _ -> []

each :: Q JSON
each =
  Kleisli $ \case
    Aeson.Object m -> HashMap.elems m
    Aeson.Array v -> toList v
    _ -> []

