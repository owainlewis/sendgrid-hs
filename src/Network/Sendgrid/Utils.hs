module Network.Sendgrid.Utils
  ( urlEncode
  ) where

import Data.Char     ( digitToInt, intToDigit, toLower, isDigit,
                       isAscii, isAlphaNum )

urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t)
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (eightBs [] (fromEnum ch))
  | otherwise = escape (fromEnum ch) (urlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)
     showH x xs
       | x <= 9    = toEnum (o_0 + x) : xs
       | otherwise = toEnum (o_A + (x-10)) : xs
      where
       o_0 = fromEnum '0'
       o_A = fromEnum 'A'
     eightBs :: [Int]  -> Int -> [Int]
     eightBs acc x
      | x <= 0xff = (x:acc)
      | otherwise = eightBs ((x `mod` 256) : acc) (x `div` 256)
