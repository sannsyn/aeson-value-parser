module Aeson.ValueParser.Error
where

import Aeson.ValueParser.Prelude


data Error = Error [Text] {-^ Path -} Text {-^ Message -}

instance Semigroup Error where
  (<>) a _ = a

instance Monoid Error where
  mempty = Error [] ""
  mappend = (<>)

instance IsString Error where
  fromString = message . fromString

{-# INLINE indexed #-}
indexed :: Int -> Error -> Error
indexed = named . fromString . show

{-# INLINE named #-}
named :: Text -> Error -> Error
named name (Error path message) = Error (name : path) message

{-# INLINE message #-}
message :: Text -> Error
message = Error []
