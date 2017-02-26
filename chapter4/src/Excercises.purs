module Excercies where

import Prelude
import Control.Monad.Eff.Console (logShow)
import Control.MonadZero (guard)
import Data.Array (concatMap, filter, null, (..))
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Partial.Unsafe (unsafePartial)

length :: forall a. Array a -> Int
length arr =
  if null arr
     then 0
     else 1 + length (unsafePartial tail arr)

countEvens :: Array Int -> Int
countEvens arr =
  if null arr
     then 0
     else if mod j 2 == 0
     then 1 + tailCount
     else 0 + tailCount
       where
         j = unsafePartial head arr
         tailCount = countEvens (unsafePartial tail arr)

infix 8 filter as <$?>
removeNegatives :: Array Int -> Array Int
removeNegatives arr =
  isPositive <$?> arr
    where
      isPositive j = j >= 0

factors' :: Int -> Array (Array Int)
factors' n = filter (\pair -> product pair == n) (pairs n)
  where
    pairs p =
      concatMap (\i ->
        map (\j -> [i, j]) (i .. p)
      ) (1 .. p)

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i,j]

pairs :: Int -> Array (Array Int)
pairs n = do
  i <- 1 .. n
  j <- i .. n
  pure [i,j]

factors'' :: Int -> Array (Array Int)
factors'' n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

factorizations :: Int -> Array (Array Int)
factorizations n = do
  i <- 1 .. (n-1)
  guard $ mod n i == 0
  pure [n/i, i]

