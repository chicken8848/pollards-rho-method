module Pollards where

import Control.Monad (when)
import Text.Printf
import Data.Bits
import Data.List (find)

modExp :: Int -> Int -> Int -> Int
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1

mix_up :: Int -> Int -> Int -> Int -> Int
mix_up x p g h
  | 0 <= x && x < (p `div` 3)      = (g * x) `mod` p
  | (p `div` 3) <= x && x < (2 * p `div` 3) = (x^2) `mod` p
  | (2 * p `div` 3) <= x && x < p  = (h * x) `mod` p
  | otherwise                      = 0

alpha_power :: Int -> Int -> Int -> Int
alpha_power a x p
  | 0 <= x && x < (p `div` 3) = ((a + 1) `mod` (p-1))
  | (p `div` 3) <= x && x < (2 * p `div` 3) = ((2 * a) `mod` (p-1))
  | (2 * p `div` 3) <= x && x < p = a
  | otherwise = 0

beta_power :: Int -> Int -> Int -> Int
beta_power b x p
  | 0 <= x && x < (p `div` 3) = (b `mod` (p - 1))
  | (p `div` 3) <= x && x < (2 * p `div` 3) = ((2 * b) `mod` (p-1))
  | (2 * p `div` 3) <= x && x < p = ((b + 1) `mod` (p-1))
  | otherwise = 0

extendedEuclid :: Int -> Int -> (Int, Int, Int)
extendedEuclid a b
  | b == 0    = (a, 1, 0)
  | otherwise = let (g, x', y') = extendedEuclid b (a `mod` b)
                    x = y'
                    y = x' - (a `div` b) * y'
                in (g, x, y)

rho_method :: Int -> Int -> Int -> IO ()
rho_method g h p = loop 1 1 g h p 0 0 0 0
  where
    loop x y g h p a b c d = do
      let new_a = alpha_power a x p
      let new_b = beta_power b x p
      let y' = mix_up y p g h
      let new_c = alpha_power (alpha_power c y p) y' p
      let new_d = beta_power (beta_power d y p) y' p
      let new_x = mix_up x p g h
      let new_y = mix_up y' p g h
      if new_x == new_y
        then do
          let set = find_set (new_a - new_c) (new_d - new_b) p
          let power = check_set g h p set
          case power of
           Just power -> printf "Collision found: %d, %d^%d = %d^%d\n%d^%d = %d in F(%d)\n" new_x g ((new_a - new_c) `mod` (p-1)) h ((new_d - new_b) `mod` (p-1)) g power h p
           Nothing -> printf "Nothing"
        else loop new_x new_y g h p new_a new_b new_c new_d

find_set :: Int -> Int -> Int -> [Int]
find_set a b p = set_iterator (omega `div` d) p d
  where
      (g, x, y) = extendedEuclid b (p-1)
      d = g
      omega = ((a * x) `mod` (p-1))

set_iterator :: Int -> Int -> Int -> [Int]
set_iterator base p d = [base + i * ((p-1) `div` d) | i <- [0..(d-1)]]

check_set :: Int -> Int -> Int -> [Int] -> Maybe Int
check_set g h p set = find (\e -> (modExp g e p) == h) set
