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
  where parser inp g l k c vs fs = (rs' , first)
            where first = if S.member n vs -- If non-terminal already checked
                         then S.empty -- Return an empty set as it does not contribute
                         else snd $ p inp n k cont_for (S.insert n vs) fs -- Else continue
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
seqStart inp x beta l c _ fs = (continue inp (Slot x [] beta, l, l, l) c, fs) -- Add the incoming followset as firstset

seqOp :: Ord t => Parse_Seq t -> Parse_Symb t -> Parse_Seq t
seqOp left right = parser
        where   seq = left
                (s, symb) = right
                parser inp x beta l c0 vs fs = (command_seq, first_seq)
                    where
                    (command_symb, first_symb) = symb inp (Slot x [] beta) l l c0 vs fs -- Get Firstset from non-terminal
                    (command_seq, first_seq) = seq inp x (s:beta) l c1 vs first_symb -- Continue and return this firstset as followset for previous symbol
                        where c1 = ContF c1f
                                where c1f inp ((Slot _ alpha _),l,k) = fst $ symb inp (Slot x (alpha++[s]) beta) l k c2 vs fs
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
            where  parser inp n l c _ _ = (id, S.empty) -- Start empty alternate

-- Retrieve the character from the input we want to match.
nextchar :: Input t -> Int -> t
nextchar inp l = arr A.! l
                where   (arr, _) = inp

-- Firstset check, if epsilon its always true (backup).
isinfirst :: (Parseable t, Ord t) => t -> Firstset t -> Bool
isinfirst symbol first = S.member eps first || S.member True (boolset symbol first)

-- Matches a character with each symbol in the firstset.
boolset :: (Parseable t) => t -> Firstset t -> S.Set Bool
boolset symbol fs = S.map (matches symbol) fs

altOp :: (Parseable t, Ord t) => Parse_Choice t -> Parse_Seq t -> Parse_Choice t
altOp left right = parser
  where
    alt = left
    seq = right
    parser inp n l c vs fs = (new_command, new_first)
        where
            (command_alt, first_alt) = alt inp n l c vs fs -- Get firstset from alternative
            (command_seq, first_seq) = seq inp n [] l c vs fs -- Get firstset from sequence.
            new_first = S.union first_alt first_seq -- Combine firstset for the alternatives
            new_command = if isinfirst (nextchar inp l) first_seq -- If the next character is in the firstset
                        then command_alt . command_seq -- Then its a valid production
                        else command_alt -- Otherwise dont add it.

compAll :: [Command t] -> Command t
compAll = foldr (.) id

applyCF (ContF cf) inp a = cf inp a

predicate :: Parseable t => Nt -> (t -> Bool) -> t -> Parse_Symb t
predicate nt p t = (Nt nt, parser)
  where parser inp g l k c _ fs = (command, first)
            where   first = S.fromList [t] -- The firstset we pass on, just the terminal
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
