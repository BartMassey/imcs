--- Copyright © 2009 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "MIT License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

--- Logistic Elo rating system for IMCS.
--- http://en.wikipedia.org/wiki/Elo_rating_system

module Rating (Rating, baseRating, updateRating) where

type Rating = Int

--- rating adjustment range constant
k :: Int
k = 32

baseRating :: Rating
baseRating = 1000

updateRating :: Rating -> Rating -> Int -> Rating
updateRating ra rb outcome = ra' where
    ea = 1.0 / (1.0 + 10.0**(fromIntegral (rb - ra) / 400.0))
    sa = (fromIntegral outcome + 1.0) / 2.0
    ra' = ra + floor (fromIntegral k * (sa - ea))
