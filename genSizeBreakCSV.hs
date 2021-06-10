#!/usr/bin/env stack
-- stack --resolver lts-16.12 --install-ghc script --package filepath --package directory --package containers
module Main where

import Text.Printf

import Data.List
import Control.Applicative
import Control.Monad
import Control.Exception (assert)

data AgeCat
    = Adult
    | Child
    deriving Show

type Dist a = (a, Double)

pSLEOne = (<= 1.0) . sum . map snd

--TODO newtyping around demographic types
--DEMOGRAPHICS
regions = [("NA",0.7), ("EU",0.2), ("ASIA",0.1)]
genders = [("Male", 0.5), ("Female", 0.5)]

--BREAKS
adult_sizebreaks = zip [22, 22.5 .. 31.5] $ repeat 0.05
children_age_breaks = zip [8..17] $ repeat 0.1

--TOTALS
total_adult = 500
total_children = 500

--DISTRIBUTION BREAKS
children_categories = liftA3 (,,) regions genders children_age_breaks
adult_categories = liftA3 (,,) regions genders adult_sizebreaks

show_cat :: (AgeCat, Int) -> (Dist String, Dist String, Dist Float) -> String
show_cat (agc, total) ((region, r), (gender, g), (break, b)) =
    foldr (<>) "" $ intersperse ", " [category_tag, dist_rep, dist_total_rep]
    where
        distribution = r * g * b

        dist_rep = printf "%.6f" distribution
        break_rep = case agc of
            Adult -> printf "%fcm" break
            Child -> "Age " <> (printf "%d" (floor break :: Int) :: String)
        dist_total_rep = printf "%.5f" (fromIntegral total * distribution)

        category_tag = show agc <> " " <> region <> " " <> gender <> " " <> break_rep

main :: IO ()
main = do
    mapM_ putStrLn $ show_cat (Adult, total_adult) <$> adult_categories
    putStrLn ""
    mapM_ putStrLn $ show_cat (Child, total_children) <$> children_categories
