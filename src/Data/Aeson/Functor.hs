{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Functor
  ( ValueF(..)
  , Value(..), toValue
  , ValueM, toM, fromM
  ) where

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

import Control.Monad.Free

data ValueF a = Object (HashMap Text a)
              | Array (Vector a)
              | String Text
              | Number Scientific
              | Bool Bool
              | Null
              deriving (Show, Eq, Functor)

toF :: Aeson.Value -> ValueF Aeson.Value
toF (Aeson.Object h) = Object h
toF (Aeson.Array v) = Array v
toF (Aeson.String s) = String s
toF (Aeson.Number n) = Number n
toF (Aeson.Bool b) = Bool b
toF Aeson.Null = Null

fromF :: ValueF Aeson.Value -> Aeson.Value
fromF (Object h) = Aeson.Object h
fromF (Array v) = Aeson.Array v
fromF (String s) = Aeson.String s
fromF (Number n) = Aeson.Number n
fromF (Bool b) = Aeson.Bool b
fromF Null = Aeson.Null

data Value = Value { unF :: ValueF Value }
  deriving (Show, Eq)

instance Aeson.FromJSON Value where
  parseJSON = pure . toValue

instance Aeson.ToJSON Value where
  toJSON = toAeson

toValue :: Aeson.ToJSON a => a -> Value
toValue = Value . fmap toValue . toF . Aeson.toJSON

toAeson :: Value -> Aeson.Value
toAeson = fromF . fmap toAeson . unF

newtype ValueM a = ValueM { unM :: Free ValueF a }
  deriving (Show, Eq, Functor, Applicative, Monad)

instance Aeson.ToJSON a => Aeson.ToJSON (ValueM a) where
  toJSON = toAeson . fromM

instance Aeson.FromJSON (ValueM a) where
  parseJSON = pure . toM . toValue

fromM :: Aeson.ToJSON a => ValueM a -> Value
fromM = iter Value . unM . fmap toValue

toM :: Value -> ValueM a
toM = ValueM . fromValue'
  where fromValue' = Free . fmap fromValue' . unF
