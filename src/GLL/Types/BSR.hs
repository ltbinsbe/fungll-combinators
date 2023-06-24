{-# LANGUAGE StandaloneDeriving #-}

module GLL.Types.BSR where

import qualified    Data.Map as M
import qualified    Data.IntMap as IM
import qualified    Data.IntSet as IS 

import GLL.Types.Grammar

-- make sure that tokens are equal independent of their character level value
type SlotL t    = (Slot t, Int)                   -- slot with left extent
type PrL t      = (Prod t, Int)                     -- Production rule with left extent
type NtL        = (Nt, Int)                     -- Nonterminal with left extent

-- | 
-- Stores packed nodes using nested "Data.IntMap"s, nesting is as follows:
--
-- * left extent
-- * right extent
-- * dot position (from left to right)
-- * mapping from productions to set of pivots
type BSRs t  =   IM.IntMap (IM.IntMap (IM.IntMap (M.Map (Prod t) IS.IntSet)))
type BSR t = (Slot t, Int, Int, Int)

emptyBSRs :: (Ord t) => BSRs t
emptyBSRs = IM.empty

pNodeLookup :: (Ord t) => BSRs t -> (Slot t, Int, Int) -> Maybe [Int]
pNodeLookup bsrs (Slot x alpha beta,l,r)= pNodeLookup' bsrs ((Prod x (alpha++beta),length alpha),l,r)

pNodeLookup' :: (Ord t) => BSRs t -> ((Prod t, Int), Int, Int) -> Maybe [Int]
pNodeLookup' pMap ((alt,j),l,r) = maybe Nothing inner $ IM.lookup l pMap
    where   inner   = maybe Nothing inner2 . IM.lookup r
            inner2  = maybe Nothing inner3 . IM.lookup j
            inner3  = maybe Nothing (Just . IS.toList) . M.lookup alt

addBSR = pMapInsert
addBSR, pMapInsert :: (Ord t) => BSR t -> BSRs t -> BSRs t
pMapInsert f@((Slot x alpha beta), l, k, r) pMap = 
 add (Prod x (alpha++beta)) (length alpha) l r k
 where add alt j l r k = IM.alter addInnerL l pMap
        where addInnerL mm = case mm of 
                             Nothing -> Just singleRJAK
                             Just m ->  Just $ IM.alter addInnerR r m
              addInnerR mm = case mm of
                             Nothing -> Just singleJAK
                             Just m  -> Just $ IM.alter addInnerJ j m
              addInnerJ mm = case mm of
                             Nothing -> Just singleAK
                             Just m  -> Just $ M.insertWith IS.union alt singleK m
              singleRJAK= IM.fromList [(r, singleJAK)]
              singleJAK = IM.fromList [(j, singleAK)]
              singleAK  = M.fromList [(alt, singleK)]
              singleK   = IS.singleton k

showBSRs pMap = unlines [ show ((a,j),l,r) ++ " --> " ++ show kset
                        | (l,r2j) <- IM.assocs pMap, (r,j2a) <- IM.assocs r2j
                        , (j,a2k) <- IM.assocs j2a, (a,kset) <- M.assocs a2k ]


