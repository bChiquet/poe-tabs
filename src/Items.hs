{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Items
    ( Item (..)
    , Mod (..)
    , getItems
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value(Object), decode, (.:), parseJSON)
import Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Char (toLower, isNumber)
import Data.List (isSuffixOf)
import Data.Text (unpack)

import Mods (Mod)

data Tab = Tab
    { numTabs    :: Int
    , items      :: [Item]
    }
    deriving (Show, Generic)

instance FromJSON Tab

data Item = Item 
    { name          :: String
    , implicitMods  :: [Mod]
    , explicitMods  :: [Mod]
    , horizontal    :: Int
    , vertical      :: Int
    }
    deriving (Generic)

instance Show Item where
    show item =  "< name: "       <> show (name item)
              <> ", horizontal: " <> show (horizontal item)
              <> ", vertical: "   <> show (vertical item)
              <> " >"
instance FromJSON Item where
    parseJSON (Object item) = 
        Item <$> item .: "name"
             <*> item .: "implicitMods"
             <*> item .: "explicitMods"
             <*> item .: "x"
             <*> item .: "y"
                        
fixPosition :: Item -> Item
fixPosition item = 
    item { horizontal = horizontal item + 1
         , vertical   = vertical   item + 1
         }

getItems stashTab = 
    map fixPosition
    $ fromMaybe []
    $ fmap items
    $ (decode stashTab :: Maybe Tab)








