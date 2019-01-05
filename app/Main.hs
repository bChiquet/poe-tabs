module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Items ( getItems
             , Item
             )
import Rules (beltRules, ringRules)

main :: IO ()
main = do
    -- https://www.pathofexile.com/character-window/get-stash-items?accountName=orime&tabIndex=8&league=Betrayal
    stashTab <- readFile "../../tabs/belts2.txt"
    mapM_ print
        $ filter beltRules
        $ getItems stashTab
