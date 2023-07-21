
module GLL.Combinators.Visit.FUNGLL where

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

type Visited = [Nt]
type Firstset t = S.Set t
type Followset t = M.Map Nt (S.Set t)
type ToMatch = S.Set Nt
type FirstGramm t = Visited -> Firstset t -> Followset t -> (Firstset t)
type SetGrammar t = Visited -> Firstset t -> Followset t -> (Firstset t, Followset t, ToMatch)
type ChoiceGram t = Input t -> Nt -> Int -> ContF t -> Command t
type SeqGram t = Input t -> Nt -> [Symbol t] -> Int -> ContF t -> Command t

epsilonTerm :: (Parseable t) => Symbol t
epsilonTerm = Term eps

endofstring :: (Parseable t) => Symbol t
endofstring = Term eos

type Parse_Symb t   = (Symbol t, Input t -> Slot t -> Int -> Int -> ContF t -> Command t, SetGrammar t)
type Parse_Choice t = (Input t -> Nt -> Int -> ContF t -> Command t, SetGrammar t)
type Parse_Seq t    = (Input t -> Nt -> [Symbol t] -> Int -> ContF t -> Command t, SetGrammar t)
type Parse_Alt t    = Parse_Seq t

parser_for :: (Parseable t) => Nt -> Parse_Symb t -> Input t -> ParseResult t
parser_for x p inp = resultFromState inp (run_parse x p inp 0 emptyState)

run_parse :: Nt -> Parse_Symb t -> Input t -> Int ->
                                State t (ContF t) -> State t (ContF t)
run_parse x p@(y,pf,_) inp l = pf inp (Slot x [y] []) l l counter_cont

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

parse_seq :: (Parseable t, Ord t) => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
parse_seq = seqOp

doNterm :: (Ord t, Eq t) => Nt -> SetGrammar t -> SetGrammar t
doNterm nt grammar vs fs fol
                | nt `elem` vs = (fs, M.empty, S.fromList [nt])
                | otherwise = (first, follow, S.fromList [nt])
                where (first, follow, tomatch) = grammar (vs ++ [nt]) fs (M.unionWith (S.union) fol follow)

nterm :: (Ord t) => Nt -> Parse_Choice t -> Parse_Symb t
nterm n choice = (Nt n, parser, grammar)
  where (p, ch) = choice
        grammar = doNterm n ch
        parser inp g l k c s
          | null rs   = p inp n k cont_for s'
          | otherwise = compAll [ applyCF c (removePrefix len inp) (g,l,r)
                                | r <- rs, let len = r - k ] s'
          where s' = s { grel = addCont (n,k) (g,l,c) (grel s) }
                rs = extents (n,k) (prel s)

        cont_for = ContF cf
         where cf inp (_,k,r) s =
                compAll [ applyCF c inp (g,l',r)
                        | (g,l',c) <- conts (n,k) (grel s) ] s'
                where s' = s { prel = addExtent (n,k) r (prel s) }

getSnd (_,a,_) = a

term :: Parseable t => t -> Parse_Symb t
term t = (Term t, parser, grammar)
        where   parser = getSnd (predicate (pack (show t)) (matches t))
                grammar _ _ _ = (S.fromList [t], M.empty, S.empty)

seqStart :: Ord t => Parse_Seq t
seqStart = (parser, grammar)
            where   parser inp x beta l c = continue inp (Slot x [] beta, l, l, l) c
                    grammar _ _ _ = (S.empty, M.empty, S.empty)

seqfirst :: (Parseable t) => Firstset t -> Firstset t -> Firstset t
seqfirst fs1 fs2
    | null fs1 = nfs
    | eps `elem` fs1 = S.union nfs (S.delete eps fs1)
    | otherwise = fs1
    where nfs = fs2

                -- if null fs2
                --     then S.fromList [eos]
                --     else fs2

doseqop :: (Parseable t) => SetGrammar t -> SetGrammar t -> SetGrammar t
doseqop seq symb vs fs fol = (newfirst, newfollow, newtomatch)
                    where   (first1, follow1, tomatch1) = seq vs fs fol
                            (first2, _, _) = symb vs fs fol
                            newfirst = seqfirst first1 first2
                            newfollow = if null tomatch1
                                        then follow1
                                        else M.unionWith (S.union) follow1 (M.fromList [(S.elemAt 0 tomatch1, first2)])
                            newtomatch = tomatch1

seqOp :: (Parseable t, Ord t) => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
seqOp left right = (parser, grammar)
        where   (p, seq) = left
                (s, q, symb) = right
                grammar = doseqop seq symb
                parser inp x beta l c0 = p inp x (s:beta) l c1
                    where c1 = ContF c1f
                            where c1f inp ((Slot _ alpha _),l,k) = q inp (Slot x (alpha++[s]) beta) l k c2
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
altStart = (parser, grammar)
            where   grammar _ _ _ = (S.empty, M.empty, S.empty)
                    parser inp n l c s = s

doaltop :: (Parseable t, Ord t) => SetGrammar t -> SetGrammar t -> SetGrammar t
doaltop ch seq vs fs fol = (newfirst, newfollow, S.empty)
                    where   (first1, follow1, _) = ch vs fs fol
                            (first2, follow2, tomatch2) = seq vs fs fol
                            nfirst2 = if null first2
                                        then S.insert eps first2
                                        else S.delete eos first2
                            newfirst = S.union first1 nfirst2
                            newfollow2 = if null tomatch2
                                            then follow2
                                            else M.unionWith (S.union) follow2 (M.fromList [(S.elemAt 0 tomatch2, S.fromList [eos])])
                            newfollow = M.unionWith (S.union) follow1 newfollow2

nextchar :: Input t -> Int -> t
nextchar inp l = arr A.! l
                where   (arr, _) = inp

isinfirst :: (Parseable t, Ord t) => t -> Firstset t -> Bool
isinfirst symbol first = if symbol == eos
                        then True -- S.member eps first
                        else S.member symbol first

createparse :: (Parseable t, Ord t) =>  Firstset t -> ChoiceGram t -> SeqGram t -> ChoiceGram t
createparse first p q inp n l c = if isinfirst (nextchar inp l) first
                                    then p inp n l c . q inp n [] l c
                                    else p inp n l c

altOp :: (Parseable t, Ord t) => Parse_Choice t -> Parse_Seq t -> Parse_Choice t
altOp left right = (parser, grammar)
  where
    (p, ch) = left
    (q, seq) = right
    grammar vs fs fol = doaltop ch seq vs fs fol
        where newfirst = justFirst seq vs fs fol
    parser = createparse newfirst p q

-- altOp :: (Eq t, Parseable t, Ord t) => Parse_Choice t -> Parse_Seq t -> Parse_Choice t
-- altOp left right = (parser, grammar)
--             where   (p, ch) = left
--                     (q, seq) = right
--                     grammar = doaltop ch seq
--                     parser inp n l c = p inp n l c . q inp n [] l c
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

parse_lexical :: Nt -> RawParser t -> Parse_Symb t
parse_lexical n scanner = (Nt n, parser, grammar)
  where grammar _ _ _ = (S.empty, M.empty, S.empty)
        parser inp g l k c s =
          compAll [ applyCF c (removePrefix len inp) (g, l, k + len)
                  | prefix <- apply_scanner scanner inp
                  , let len = length prefix ] s

-- For testing purposes
justFirst :: SetGrammar t -> FirstGramm t
justFirst seq vs fs fol = first
                where (first, _, _) = seq vs fs fol

getFirstSymb :: Parse_Symb t -> Firstset t
getFirstSymb symb = first
                where (first, _, _) = grammar [] S.empty M.empty
                        where (_, _, grammar) = symb

getFirstSeq :: Parse_Seq t -> Firstset t
getFirstSeq seq = first
                where (first, _, _) = grammar [] S.empty M.empty
                        where (_, grammar) = seq

getFirstChoice :: Parse_Choice t -> Firstset t
getFirstChoice choice = first
                where (first, _, _) = grammar [] S.empty M.empty
                        where (_, grammar) = choice

{- EXPERIMENTAL -}

andNot :: (Show t) => Parse_Symb t -> Parse_Symb t -> Parse_Symb t
andNot (lnt,p, _) (rnt,q, _) = (Nt lhs_symb,parser, grammar)
  where grammar _ _ _ = (S.empty, M.empty, S.empty)
        lhs_symb = pack ("__andNot(" ++ show lnt ++","++ show rnt ++ ")")
        parser inp g l k c s = compAll [ applyCF c (removePrefix len inp) (g, l, r)
                                       | r <- rs, let len = r - k ]
                                       s2{successes = successes s}
          where s1 = run_parse lhs_symb (lnt,p,grammar) inp k s{successes = IM.empty}
                s2 = run_parse lhs_symb (rnt,q,grammar) inp k s1{successes = IM.empty}
                rs = IS.toList $ IS.difference (IM.keysSet (successes s1))
                                               (IM.keysSet (successes s2))


ands :: (Show t) => [Parse_Symb t] -> Parse_Symb t
ands = foldr andOp andStart

andOp :: (Show t) => Parse_Symb t -> Parse_Symb t -> Parse_Symb t
andOp (lnt,p, _) (rnt,q, _) = (Nt lhs_symb,parser, grammar)
  where grammar _ _ _ = (S.empty, M.empty, S.empty)
        lhs_symb = pack ("__and(" ++ show lnt ++","++ show rnt ++ ")")
        parser inp g l k c s = compAll [ applyCF c (removePrefix len inp) (g, l, r)
                                       | r <- rs, let len = r - k ] s2
          where s1 = run_parse lhs_symb (lnt,p, grammar) inp k s
                s2 = run_parse lhs_symb (rnt,q, grammar) inp k s1
                rs = IS.toList $ IS.intersection (IM.keysSet (successes s1))
                                                 (IM.keysSet (successes s2))

andStart :: Parse_Symb t
andStart = (Nt (pack "__and_unit"), parser, grammar)
  where grammar _ _ _ = (S.empty, M.empty, S.empty)
        parser inp g l k c s = applyCF c inp (g, l, k) s

predicate :: Parseable t => Nt -> (t -> Bool) -> Parse_Symb t
predicate nt p = (Nt nt, parser, grammar)
  where grammar _ _ _ = (S.empty, M.empty, S.empty)
        parser inp g l k c s =
          compAll [ applyCF c (removePrefix len inp) (g, l, k + len)
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
