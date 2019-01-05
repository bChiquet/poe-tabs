{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Rules
    ( ringRules
    , beltRules
    ) where

import Items (Item, explicitMods)
import Mods ( Mod(..)
            , ResistType(..)
            )

type Score = Int

beltRules = ringRules

ringRules :: Item -> Bool
ringRules item = score item >= 5
{-
       (hpAbove 60 item && resistsAbove 60 item)
    || (hpAbove 50 item && resistsAbove 30 item && pctDamageAbove 20 item)
    || (hpAbove 50 item && resistsAbove 30 item && pctDamageAbove 20 item)
    || (resistsAbove 80 item)
    || (hpAbove 50 item && resistsAbove 30 item &&  flatAverageDamageAbove 25 item)
-}

score :: Item -> Score
score item = poolScore item
           + resistScore item
           + damageScore item
           + flatDamageScore item

poolScore :: Item -> Score
poolScore item =
    if      hpAbove 75 item then 2
    else if esAbove 37 item then 2
    else if hpAbove 60 item then 1
    else if esAbove 31 item then 1
    else if craftablePool item then 1
    else 0

resistScore :: Item -> Score
resistScore item =
    if      resistsAbove 100 item then 4
    else if resistsAbove 70  item then 3
    else if resistsAbove 40  item then 2
    else 0
    
damageScore :: Item -> Score
damageScore item =
    if      pctDamageAbove 45 item then 5
    else if pctDamageAbove 30 item then 3
    else if pctDamageAbove 20 item then 1
    else 0
    
    
flatDamageScore :: Item -> Score
flatDamageScore item = 
    if      flatAverageDamageAbove 25 item then 4
    else if flatAverageDamageAbove 20 item then 3
    else if flatAverageDamageAbove 10 item then 1
    else 0

 
---------- Helpers
 
 
 
 
 
hpAbove :: Int -> Item -> Bool
hpAbove required item = 
    let hps (Life value) = value
        hps _            = 0
    in (sum $ map hps (explicitMods item) ) > required
    
esAbove :: Int -> Item -> Bool
esAbove required item = 
    let es (ES value) = value
        es _            = 0
    in (sum $ map es (explicitMods item) ) > required
    
craftablePool :: Item -> Bool
craftablePool item = not (hpAbove 0 item && esAbove 0 item)
    
resistsAbove :: Int -> Item -> Bool
resistsAbove required item = 
    let resistValue (Resist All value) = value * 3
        resistValue (Resist _   value) = value
        resistValue _                  = 0
    in (sum $ map resistValue (explicitMods item)) > required
    
pctDamageAbove :: Int -> Item -> Bool
pctDamageAbove required item = 
    let damageMods (PctEleAttackDamage value) = value
        damageMods (PctDamage _ value)        = value
        damageMods _                          = 0
    in (sum $ map damageMods (explicitMods item)) > required
    
flatAverageDamageAbove :: Int -> Item -> Bool
flatAverageDamageAbove required item =
    let flatDamage (FlatDamage _ min max)   = quot (min + max) 2
        flatDamage (FlatPhysDamage min max) = quot (min + max) 2
        flatDamage _                        = 0
    in (sum $ map flatDamage (explicitMods item)) > required
    