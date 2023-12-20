
module GLL.Combinators.Visit.FUNGLL where

import Debug.Trace -- DEBUGGING

import GLL.Types.Grammar
import GLL.Types.BSR
import GLL.Types.DataSets
import GLL.Types.Input

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Array as A
import Data.Text (pack)

type Command t  = State t (ContF t) -> State t (ContF t)
data ContF t    = ContF (Input t -> Descr t -> Command t)

type Firstset t = S.Set t
type Visited = S.Set Nt

type Parse_Symb t   = (Symbol t, Input t -> Slot t -> Int -> Int -> ContF t -> Visited -> Firstset t -> (Command t, Firstset t))
type Parse_Choice t = Input t -> Nt -> Int -> ContF t -> Visited -> Firstset t -> (Command t, Firstset t)
type Parse_Seq t    = Input t -> Nt -> [Symbol t] -> Int -> ContF t -> Visited -> Firstset t -> (Command t, Firstset t)
type Parse_Alt t    = Parse_Seq t

parser_for :: (Parseable t) => Nt -> Parse_Symb t -> Input t -> ParseResult t
parser_for x p inp = resultFromState inp (run_parse x p inp 0 emptyState)

run_parse :: (Parseable t, Ord t) => Nt -> Parse_Symb t -> Input t -> Int ->
                                State t (ContF t) -> State t (ContF t)
run_parse x p@(y,pf) inp l = fst $ pf inp (Slot x [y] []) l l counter_cont S.empty (S.fromList [eps])

counter_cont :: ContF t
counter_cont = ContF cf
  where cf _ (_,_,r) s = s { successes = IM.alter updater r (successes s) }
          where updater = maybe (Just 1) (Just . (1+))

parse_nterm :: (Parseable t, Ord t) => Nt -> [Parse_Seq t] -> Parse_Symb t
parse_nterm n = nterm n . foldl altOp altStart

parse_term :: Parseable t => t -> Parse_Symb t
parse_term = term

parse_apply :: (Parseable t, Ord t) => Parse_Symb t -> Parse_Seq t
parse_apply = seqOp seqStart

parse_seq :: Ord t => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
parse_seq = seqOp

nterm :: (Parseable t, Ord t) => Nt -> Parse_Choice t -> Parse_Symb t
nterm n p = (Nt n, parser)
  where parser inp g l k c vs fs = (rs' , gres)
            where gres = if S.member n vs
                         then S.fromList [eps]
                         else snd $ p inp n k cont_for (S.insert n vs) fs
                  rs' s | null rs   = fst (p inp n k cont_for vs fs) s'
                        | otherwise = compAll [ applyCF c (removePrefix len inp) (g,l,r)
                                            | r <- rs, let len = r - k ] s'
                    where   s' = s { grel = addCont (n,k) (g,l,c) (grel s) }
                            rs = extents (n,k) (prel s)

        cont_for = ContF cf
         where cf inp (_,k,r) s =
                compAll [ applyCF c inp (g,l',r)
                        | (g,l',c) <- conts (n,k) (grel s) ] s'
                where s' = s { prel = addExtent (n,k) r (prel s) }

term :: Parseable t => t -> Parse_Symb t
term t = (Term t, snd (predicate (pack (show t)) (matches t) t))

seqStart :: (Parseable t, Ord t) => Parse_Seq t
seqStart inp x beta l c _ fs = (continue inp (Slot x [] beta, l, l, l) c, fs)

-- seqOp :: Ord t => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
-- seqOp p (s,q) inp x beta l c0 = p inp x (s:beta) l c1
--   where c1 = ContF c1f
--          where c1f inp ((Slot _ alpha _),l,k) = q inp (Slot x (alpha++[s]) beta) l k c2
--                 where c2 = ContF c2f
--                        where c2f inp (g,l,r) = continue inp (g,l,k,r) c0

seqOp :: Ord t => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
seqOp left right = parser
        where   p = left
                (s, q) = right
                parser inp x beta l c0 vs fs = (command_p, first_p)
                    where
                    (command_q, first_q) = q inp (Slot x [] beta) l l c0 vs fs
                    (command_p, first_p) = p inp x (s:beta) l c1 vs first_q
                        where c1 = ContF c1f
                                where c1f inp ((Slot _ alpha _),l,k) = fst $ q inp (Slot x (alpha++[s]) beta) l k c2 vs fs
                                        where c2 = ContF c2f
                                                where c2f inp (g,l,r) = continue inp (g,l,k,r) c0

continue :: (Ord t) => Input t -> BSR t -> ContF t -> Command t
continue inp bsr@(g@(Slot x alpha beta),l,k,r) c s
  | hasDescr descr (uset s) = s'
  | otherwise               = applyCF c inp descr s''
  where descr = (g,l,r)
        s'  | not (null alpha) || null beta = s { bsrs = addBSR bsr (bsrs s) }
            | otherwise                     = s
        s'' = s' { uset = addDescr descr (uset s') }

altStart :: Parse_Choice t
altStart = parser
            where  parser inp n l c _ _ = (id, S.empty)

nextchar :: Input t -> Int -> t
nextchar inp l = arr A.! l
                where   (arr, _) = inp

-- nextchar :: Input t -> Int -> t
-- nextchar inp l = list !! l
--                 where   (arr, list) = inp


isinfirst :: (Parseable t, Ord t) => t -> Firstset t -> Bool
isinfirst symbol first = S.member eps first || S.member symbol first

altOp :: (Parseable t, Ord t) => Parse_Choice t -> Parse_Seq t -> Parse_Choice t
altOp left right = parser
  where
    p = left
    q = right
    parser inp n l c vs fs = (command, newfirst)
        where
            (command_p, first_p) = p inp n l c vs fs
            (command_q, first_q) = q inp n [] l c vs fs
            newfirst = S.union first_p first_q
            command = traceShow (nextchar inp l, first_q, isinfirst (nextchar inp l) first_q) (if isinfirst (nextchar inp l) first_q
                        then command_p . command_q
                        else command_p)
            -- command = if isinfirst (nextchar inp l) first_q
            --             then command_p . command_q
            --             else command_p

-- first_to_char :: Firstset t -> [Char]
-- first_to_char fs =

-- t_to_string :: t -> [Char]
-- t_to_string =

{- MUCH SLOWER ?
altOp p q inp n l c s =
  let s1 = p inp n l counter_cont s
      s2 = q inp n [] l counter_cont s1
  in compAll [ applyCF c (error "cont_for assert", l, r)
             | r <- IS.toList (IS.union (IM.keysSet (successes s1))
                                        (IM.keysSet (successes s2))) ] s2
-}

compAll :: [Command t] -> Command t
compAll = foldr (.) id

applyCF (ContF cf) inp a = cf inp a

{- EXTENSIONS -}

-- parse_lexical :: Nt -> RawParser t -> Parse_Symb t
-- parse_lexical n scanner = (Nt n, parser)
--   where parser inp g l k c s =
--           compAll [ applyCF c (removePrefix len inp) (g, l, k + len)
--                   | prefix <- apply_scanner scanner inp
--                   , let len = length prefix ] s

{- EXPERIMENTAL -}

-- andNot :: (Show t) => Parse_Symb t -> Parse_Symb t -> Parse_Symb t
-- andNot (lnt,p) (rnt,q) = (Nt lhs_symb,parser)
--   where lhs_symb = pack ("__andNot(" ++ show lnt ++","++ show rnt ++ ")")
--         parser inp g l k c s = compAll [ applyCF c (removePrefix len inp) (g, l, r)
--                                        | r <- rs, let len = r - k ]
--                                        s2{successes = successes s}
--           where s1 = run_parse lhs_symb (lnt,p) inp k s{successes = IM.empty}
--                 s2 = run_parse lhs_symb (rnt,q) inp k s1{successes = IM.empty}
--                 rs = IS.toList $ IS.difference (IM.keysSet (successes s1))
--                                                (IM.keysSet (successes s2))


-- ands :: (Show t) => [Parse_Symb t] -> Parse_Symb t
-- ands = foldr andOp andStart

-- andOp :: (Show t) => Parse_Symb t -> Parse_Symb t -> Parse_Symb t
-- andOp (lnt,p) (rnt,q) = (Nt lhs_symb,parser)
--   where lhs_symb = pack ("__and(" ++ show lnt ++","++ show rnt ++ ")")
--         parser inp g l k c s = compAll [ applyCF c (removePrefix len inp) (g, l, r)
--                                        | r <- rs, let len = r - k ] s2
--           where s1 = run_parse lhs_symb (lnt,p) inp k s
--                 s2 = run_parse lhs_symb (rnt,q) inp k s1
--                 rs = IS.toList $ IS.intersection (IM.keysSet (successes s1))
--                                                  (IM.keysSet (successes s2))

-- andStart :: Parse_Symb t
-- andStart = (Nt (pack "__and_unit"), parser)
--   where parser inp g l k c s = applyCF c inp (g, l, k) s

predicate :: Parseable t => Nt -> (t -> Bool) -> t -> Parse_Symb t
predicate nt p t = (Nt nt, parser)
  where parser inp g l k c _ fs = (command, first)
            where   first = S.fromList [t]-- The firstset we pass on, just the terminal
                    command s =  compAll [ applyCF c (removePrefix len inp) (g, l, k + len)
                                | prefix <- apply_scanner (scanner_from_predicate p) inp
                                , let len = length prefix ] s

-- |
-- The "ParseResult" datatype contains some information about a parse:
--
--  * Whether the parse was successful
--  * The number of descriptors that have been processed
--  * The number of BSR elements
data ParseResult t = ParseResult{ bsrs_result               :: BSRs t
                                , res_success               :: Bool
                                , res_successes             :: IM.IntMap Int
                                , nr_descriptors            :: Int
                                , nr_bsrs                   :: Int
                                , error_message             :: String
                                }

matchedUpTo :: IM.IntMap Int -> Int -> Bool
matchedUpTo res r = maybe False (const True) (IM.lookup r res)

resultFromState :: Parseable t => Input t -> State t c -> ParseResult t
resultFromState inp (State uset _ _ pMap cs) =
    let usize       = sum  [ S.size s   | (l, r2s) <- IM.assocs uset
                                        , (r,s) <- IM.assocs r2s ]
        p_nodes     = sum [ IS.size ks  | (l, r2j) <- IM.assocs pMap
                                        , (r, j2s) <- IM.assocs r2j
                                        , (j, s2k) <- IM.assocs j2s
                                        , (s, ks)  <- M.assocs s2k ]
        succs = maybe 0 id (IM.lookup (inputLength inp) cs)
    in ParseResult pMap (succs > 0) cs usize p_nodes "no errors to report"

instance Show (ParseResult t) where
    show res | res_success res = result_string
             | otherwise       = result_string ++ "\n" ++ error_message res
     where result_string = unlines $
                [   "Success             "  ++ show (res_success res)
                ,   "#Success            "  ++ show (res_successes res)
                ,   "Descriptors:        "  ++ show (nr_descriptors res)
                ,   "BSRs:               "  ++ show (nr_bsrs res)
                ]
