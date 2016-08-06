{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype Kitten = Kitten Text
               deriving Show

instance ToJSON Kitten where
  toJSON (Kitten s) =  String s

kittenKey :: Text
kittenKey = "kitten"

data Direction = MvUp
               | MvDown
               | MvLeft
               | MvRight

instance ToJSON Direction where
  toJSON MvUp = "U" :: Value
  toJSON MvDown = "D" :: Value
  toJSON MvLeft = "L" :: Value
  toJSON MvRight = "R" :: Value

dirKey :: Text
dirKey = "direction"

data Action =
  Move {
    kitten :: Kitten
  , direction :: Direction
  } deriving (Generic)

instance ToJSON Action where
  toJSON (Move k d) =
    object [kittenKey .= k,
            dirKey .= d]
