module HW03.Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = [every n xs | n <- [1..length xs]]
  where every n xs' = case drop (n-1) xs' of
          []     -> []
          (y:ys) -> y : every n ys

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) | y > x && y > z = y : localMaxima (y:z:xs)
                       | otherwise      =     localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines . transpose . fmap (reverse . plot) . freqs $ xs
  where freqs ns = fmap (\n -> (n, length . filter (==n) $ ns)) [0..9]
        plot (n, f) = take (2 + length xs) $ show n ++ "=" ++ replicate f '*' ++ repeat ' '
