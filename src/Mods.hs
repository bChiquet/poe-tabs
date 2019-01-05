{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mods
    ( Mod (..)
    , ResistType(..)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value(Object), decode, (.:), parseJSON)
import Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Char (toLower, isNumber)
import Data.List (isSuffixOf, isInfixOf)
import Data.List.Split (splitOn)
import Data.Text (unpack)

data ResistType = Against Element
                | All
    deriving (Show, Generic)

type Min = Int
type Max = Int

data Element = Fire
             | Cold
             | Lightning
             | Chaos
    deriving (Show, Generic)

data Mod = Life Int
         | ES Int
         | Resist ResistType Int
         | PctEleAttackDamage Int
         | PctDamage Element Int
         | FlatDamage Element Min Max
         | FlatPhysDamage Min Max
         | Other
    deriving (Show, Generic)

instance FromJSON Mod where
    parseJSON (Aeson.String mod_) = 
        let mod = map toLower $ unpack mod_ in
        
        -- Pool
        if      "to maximum life" `isSuffixOf` mod
            then pure $ Life $ read (takeWhile isNumber $ drop 1 mod)
        else if "to maximum energy shield" `isSuffixOf` mod
            then pure $ ES $ read (takeWhile isNumber $ drop 1 mod)
            
        -- Resists
        else if "all elemental resistances" `isSuffixOf` mod
            then pure $ Resist All  $ read (takeWhile isNumber $ drop 1 mod)
        else if "to fire resistance" `isSuffixOf` mod
            then pure $ Resist (Against Fire)  $ read (takeWhile isNumber $ drop 1 mod)
        else if "to cold resistance" `isSuffixOf` mod
            then pure $ Resist (Against Cold)  $ read (takeWhile isNumber $ drop 1 mod)
        else if "to lightning resistance" `isSuffixOf` mod
            then pure $ Resist (Against Lightning) $ read (takeWhile isNumber $ drop 1 mod)
        else if "to chaos resistance" `isSuffixOf` mod
            then pure $ Resist (Against Chaos)  $ read (takeWhile isNumber $ drop 1 mod)
            
        -- Damage
        else if "increased elemental damage with attack skills" `isInfixOf` mod
            then pure $ PctEleAttackDamage $ read (takeWhile isNumber $ mod)
        else if "increased fire damage" `isInfixOf` mod
            then pure $ PctDamage Fire $ read (takeWhile isNumber $ mod)
        else if "increased Cold damage" `isInfixOf` mod
            then pure $ PctDamage Cold $ read (takeWhile isNumber $ mod)
        else if "increased lightning damage" `isInfixOf` mod
            then pure $ PctDamage Lightning $ read (takeWhile isNumber $ mod)
        
        -- Flat damage
        else if "physical damage to attacks" `isInfixOf` mod
            then pure $ FlatPhysDamage 
                (read (takeWhile isNumber $ dropUntilAfter "adds " mod))
                (read (takeWhile isNumber $ dropUntilAfter "to " mod))
        else if "fire damage to attacks" `isInfixOf` mod
            then pure $ FlatDamage Fire 
                (read (takeWhile isNumber $ dropUntilAfter "adds " mod))
                (read (takeWhile isNumber $ dropUntilAfter "to " mod))
        else if "cold damage to attacks" `isInfixOf` mod
            then pure $ FlatDamage Cold 
                (read (takeWhile isNumber $ dropUntilAfter "adds " mod))
                (read (takeWhile isNumber $ dropUntilAfter "to " mod))
        else if "lightning damage to attacks" `isInfixOf` mod
            then pure $ FlatDamage Lightning 
                (read (takeWhile isNumber $ dropUntilAfter "adds " mod))
                (read (takeWhile isNumber $ dropUntilAfter "to " mod))
        else if "chaos damage to attacks" `isInfixOf` mod
            then pure $ FlatDamage Chaos 
                (read (takeWhile isNumber $ dropUntilAfter "adds " mod))
                (read (takeWhile isNumber $ dropUntilAfter "to " mod))
            
        --Mod is not known (or irrelevant)
        else pure $ Other

-- Drops all characters until and including the first appearance of the sequence.
dropUntilAfter :: String -> String -> String
dropUntilAfter seq string = splitOn seq string !! 1
