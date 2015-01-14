module JSONQuery where

import BasePrelude hiding (bool)
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector


type JSON = 
  Aeson.Value

newtype Q a =
  Q (JSON -> Either Text a)
  deriving (Functor)

instance Applicative Q where
  pure = 
    Q . const . Right
  (<*>) a b =
    Q $ \j -> ($) <$> run a j <*> run b j

instance Monad Q where
  return = 
    pure
  (>>=) m k = 
    join $ fmap k m

instance Alternative Q where
  empty = 
    Q $ const $ Left "No result"
  (<|>) a b =
    Q $ \j -> 
      case run a j of
        Left _ -> run b j
        r -> r

instance MonadPlus Q where
  mzero = empty
  mplus = (<|>)

run :: Q a -> JSON -> Either Text a
run = \(Q f) -> f

value :: Text -> Q JSON
value name =
  Q $ \case
    Aeson.Object m -> 
      maybe (Left $ "Object contains no field '" <> name <> "'") Right $
      HashMap.lookup name m
    _ ->
      Left "Not an object"

element :: Int -> Q JSON
element index =
  Q $ \case
    Aeson.Array v ->
      maybe (Left $ "Array has no index '" <> (fromString . show) index <> "'") Right $
      v Vector.!? index
    _ ->
      Left "Not an array"

string :: Q Text
string =
  Q $ \case
    Aeson.String t ->
      Right t
    _ ->
      Left "Not a string"

number :: Q Scientific
number =
  Q $ \case
    Aeson.Number x ->
      Right x
    _ ->
      Left "Not a number"

bool :: Q Bool
bool =
  Q $ \case
    Aeson.Bool x -> 
      Right x
    _ -> 
      Left "Not a bool"

nullable :: Q a -> Q (Maybe a)
nullable q =
  Q $ \case
    Aeson.Null ->
      Right Nothing
    x -> 
      fmap Just $ run q x

arrayOf :: Q a -> Q (Vector.Vector a)
arrayOf q =
  Q $ \case
    Aeson.Array v ->
      Vector.mapM (run q) v
    _ ->
      Left "Not an array"

objectOf :: Q a -> Q (HashMap.HashMap Text a)
objectOf q =
  Q $ \case
    Aeson.Object m ->
      mapM (run q) m
    _ ->
      Left "Not an object"

fromJSON :: Aeson.FromJSON a => Q a
fromJSON =
  Q $
    Aeson.fromJSON >>> \case
      Aeson.Error m -> Left $ fromString m
      Aeson.Success r -> Right $ r
