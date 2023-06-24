
module GLL.Types.DataSets where

import GLL.Types.Grammar
import GLL.Types.BSR

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

type Descr t    = (Slot t, Int, Int)
type Comm t     = (Nt, Int)
data Cont t c   = Cont (Slot t, Int) c
data State t c  = State { uset        :: USet t
                        , grel        :: GRel t c
                        , prel        :: PRel t
                        , bsrs        :: BSRs t
                        , successes   :: IM.IntMap Int {- maps index to counter -}
                        }

instance (Ord t) => Ord (Cont t c) where
  (Cont c _) `compare` (Cont c' _) = c `compare` c'

instance (Eq t) => Eq (Cont t c) where
  (Cont c _) == (Cont c' _) = c == c'

emptyUSet       :: USet t
addDescr        :: (Ord t) => Descr t  -> USet t   -> USet t
hasDescr        :: (Ord t) => Descr t  -> USet t   -> Bool

emptyG         :: GRel t c
addCont         :: (Ord t) => Comm t -> (Slot t, Int, c) -> GRel t c -> GRel t c
conts           :: Comm t -> GRel t c -> [(Slot t, Int, c)]

emptyP         :: PRel t
addExtent       :: Comm t -> Int -> PRel t -> PRel t
extents         :: Comm t -> PRel t -> [Int]

emptyState :: (Ord t) => State t c
emptyState = State emptyUSet emptyG emptyP emptyBSRs IM.empty

type RList t    =   [Descr t]
type USet t     =   IM.IntMap (IM.IntMap (S.Set (Slot t)))
type GRel t c   =   IM.IntMap (M.Map Nt (S.Set (Cont t c)))
type PRel t     =   IM.IntMap (M.Map Nt [Int])

descrs2list :: USet t -> [(Slot t, Int, Int)]
descrs2list uset =  [ (g,l,k)
                    | (l, k2m)  <- IM.assocs uset
                    , (k, g2m)  <- IM.assocs k2m
                    , g         <- S.toList g2m ]

printDescrs :: (Show t) => USet t -> IO ()
printDescrs = putStr . unlines . map show . descrs2list

emptyRList = []
popRList (x:xs)  = (x,xs) 
popRList _       = error "popRList"
unionRList       = flip (++)
singletonRList   = (:[])
fromListRList    :: Ord t => [Descr t]  -> USet t   -> RList t
fromListRList ds uset = foldr op emptyRList (nub ds)
  where op d rset   | hasDescr d uset   = rset
                    | otherwise         = unionRList (singletonRList d) rset


emptyUSet = IM.empty

addDescr alt@(slot,i,l) = IM.alter inner i 
  where inner mm = case mm of 
                      Nothing -> Just $ IM.singleton l single 
                      Just m  -> Just $ IM.insertWith (S.union) l single m
        single = S.singleton slot

hasDescr alt@(slot,i,l) = not . maybe True inner . IM.lookup i
  where inner m = maybe True (not . (slot `S.member`)) $ IM.lookup l m

emptyG = IM.empty
singleCG k v = addCont k v emptyG
addCont (n,i) (gs,l,c) = IM.alter inner i
 where inner mm = case mm of 
                    Nothing -> Just $ M.singleton n single 
                    Just m  -> Just $ M.insertWith S.union n single m
       single = S.singleton (Cont (gs,l) c)
conts (n,l) = maybe [] inner . IM.lookup l
         where inner m = maybe [] (map unCont . S.toList) $ M.lookup n m
               unCont (Cont (gs,l') cf) = (gs,l',cf)

emptyP = IM.empty
addExtent (gs,l) i = IM.alter inner l
 where inner mm = case mm of 
                    Nothing -> Just $ M.singleton gs [i]
                    Just m  -> Just $ M.insertWith (++) gs [i] m

extents (gs,l) = maybe [] inner . IM.lookup l 
         where inner = maybe [] id .  M.lookup gs
