module Utils.List
  ( select,
    selectIxs,
    splitAtMany,
  )
where

import qualified Control.Monad as Monad
import qualified Data.List as List

splitAtMany :: [Int] -> [a] -> Maybe [[a]]
splitAtMany lengths xs =
  if length xs > sum lengths
    then Just (go lengths xs [])
    else Nothing
  where
    go [] xs' acc = acc <> [xs']
    go (l : ls) xs' acc =
      let (x, rest) = List.splitAt l xs'
       in go ls rest (x : acc)

selectIxs :: [Int] -> [a] -> Maybe ([a], [a])
selectIxs ixs xs =
  if all (\ix -> length xs > ix) ixs
    then Just (matched, rest)
    else Nothing
  where
    matched = map (xs !!) ixs
    rest = do
      (i, x) <- List.zip [0 ..] xs
      Monad.guard (i `notElem` ixs)
      pure x

-- Kind of like a "strict difference"? If the first list is not a subset of
-- second list, this fails, otherwise it returns all elements of the second
-- list that aren't elements of the first list
select :: Eq a => [a] -> [a] -> Maybe [a]
select xs ys =
  if all (`elem` ys) xs
    then Just (filter (`notElem` xs) ys)
    else Nothing
