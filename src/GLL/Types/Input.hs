
module GLL.Types.Input (
  module GLL.Types.Input) where

import GLL.Types.Grammar
import qualified    Data.Array as A

type Input t      = (A.Array Int t, [t])
type RawParser t  = [t] -> [[t]]  -- input to list of prefixes

mkInput :: (Parseable t) => [t] -> Input t
mkInput input = (A.listArray (0,m) (input++[eos]), input)
  where m = length input

inputLength :: Input t -> Int
inputLength = snd . A.bounds . fst

scanner_from_predicate :: (t -> Bool) -> RawParser t
scanner_from_predicate p (t:ts) | p t = [[t]]
scanner_from_predicate p _ = []

apply_scanner :: RawParser t -> Input t -> [[t]]
apply_scanner scanner (_,str) = scanner str

slice :: Input t -> Int -> Int -> [t]
slice (arr, str) l r 
  | lb <= l && r <= ub  = map (arr A.!) [l..r-1] 
  | otherwise           = []
  where (lb,ub) = A.bounds arr

removePrefix :: Int -> Input t -> Input t
removePrefix l (arr, str) = (arr, drop l str)
