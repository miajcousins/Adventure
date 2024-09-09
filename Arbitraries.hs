module Arbitraries where

import World

import Test.QuickCheck
import Data.List (nub)

instance Arbitrary Object where
    arbitrary = elements [mug, fullmug, coffeepot, laptop, toothbrush, jeans, hoodie, trainers, keys]

instance Arbitrary Room where
    arbitrary = elements [bedroom, bathroom, wardrobe, hall, kitchen, livingroom]

instance Arbitrary Direction where 
    arbitrary = elements [North, South, East, West, Out, Up, Down]

generateInv :: Int -> Gen [Object]
generateInv n =  nub <$> vectorOf n arbitrary

instance Arbitrary GameData where
    arbitrary = do 
        locationId <- elements [x | (x, _) <- gameworld]
        inventory <- generateInv 5
        poured <- arbitrary :: Gen Bool
        caffeinated <- arbitrary :: Gen Bool
        lightsOn <- arbitrary :: Gen Bool
        dressed <- arbitrary :: Gen Bool
        finished <- arbitrary :: Gen Bool
        gotKeys <- arbitrary :: Gen Bool
        brushed <- arbitrary :: Gen Bool
        gameDark <- arbitrary :: Gen Bool 
        return (GameData locationId gameworld inventory poured caffeinated lightsOn dressed finished gotKeys brushed gameDark)