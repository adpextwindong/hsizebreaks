#!/usr/bin/env stack
-- stack --resolver lts-16.12 --install-ghc script --package filepath --package directory --package containers
module Main where

import Text.Printf
import Control.Applicative
import Control.Monad

data AgeCat
    = Adult
    | Child
    deriving Show

type Dist a = (a, Double)

--TODO newtyping around demographic types
regions = [("NA",0.7), ("EU",0.2), ("ASIA",0.1)]
genders = [("Male", 0.5), ("Female", 0.5)]

sizebreaks = zip [22, 22.5 .. 32] $ repeat 0.05

total_adult = 500

adult_categories = liftA3 (,,) regions genders sizebreaks

show_cat :: (AgeCat, Int) -> (Dist String, Dist String, Dist Float) -> String
show_cat (agc, total) ((region, r), (gender, g), (break, b)) = show agc <> " " <> region <> " " <> gender <> " " <> break_rep <> ", " <> dist_rep <> ", " <> dist_total_rep
    where
        distribution = r * g * b
        dist_rep = printf "%.5f" distribution
        break_rep = case agc of
            Adult -> printf "%fcm" break
            Child -> "Age " <> (printf "%d" (floor break :: Int) :: String)
        dist_total_rep = printf "%.5f" (fromIntegral total * distribution)

main :: IO ()
main = do
    mapM_ putStrLn $ show_cat (Adult, total_adult) <$> adult_categories
    putStrLn ""
    mapM_ putStrLn $ show_cat (Child, total_children) <$> children_categories

total_children = 500
children_breaks = zip [8..18] $ repeat 0.1

children_categories = liftA3 (,,) regions genders children_breaks
