#!/usr/bin/env stack
-- stack --resolver lts-16.12 --install-ghc script --package filepath --package directory --package containers
module Main where

import Control.Applicative
import Control.Monad

regions = ["NA", "EU", "ASIA"]
genders = ["Male", "Female"]

sizebreaks = [22, 22.5 .. 32]

adult_categories = liftA3 (,,) regions genders sizebreaks

break_cat :: (Show a) => (String, String, a) -> String
break_cat (region, gender, break) = region <> " " <> gender <> " " <> show break

ts = break_cat <$> adult_categories

main :: IO ()
main = mapM_ putStrLn ts
