{-# LANGUAGE FlexibleInstances #-}

module GLL.Combinators.Visit.Join where

import GLL.Types.Grammar
import GLL.Types.Input
import GLL.Types.TypeCompose
import GLL.Combinators.Visit.Sem
import GLL.Combinators.Visit.FUNGLL
import GLL.Combinators.Options

import Data.List (intercalate)
import Data.Text (pack)

-- | A combinator expression representing a symbol.
-- A 'SymbExpr' either represents a terminal or a nonterminal.
-- In the latter case it is constructed with (a variant of) '<:=>' and
-- adds a rule to the grammar of which the represented symbol is the
-- left-hand side.
data SymbExpr t a = SymbExpr (Symbol t, Parse_Symb t, Sem_Symb t a)
-- | A combinator expression representing a BNF-grammar. The terminals of
-- the grammar are of type 't'. When used to parse, the expression yields
-- semantic results of type 'a'.
type BNF t a = SymbExpr t a
-- |
-- A combinator expression representing an alternative:
-- the right-hand side of a production.
data AltExpr t a = AltExpr ([Symbol t], Parse_Alt t, Sem_Alt t a)

-- | A list of alternatives represents the right-hand side of a rule.
type AltExprs = OO [] AltExpr

mkNtRule :: (Parseable t, Show t, Ord t, HasAlts b) => Bool -> Bool -> String -> b t a -> SymbExpr t a
mkNtRule use_ctx left_biased x' altPs' =
    let vas1 = map (\(AltExpr (f,_,_)) -> f) altPs
        vas2 = map (\(AltExpr (_,s,_)) -> s) altPs
        vas3 = map (\(AltExpr (_,_,t)) -> t) altPs
        alts  = map (Prod x) vas1
        altPs = altsOf altPs'
        x     = pack x'
    in SymbExpr (Nt x, parse_nterm x vas2, sem_nterm use_ctx left_biased x alts vas3)

join_apply :: (Parseable t, Show t, Ord t, IsSymbExpr s, Foldable f) =>
                (a -> f b) -> s t a -> AltExpr t b
join_apply f p' =
    let SymbExpr (vpa1,vpa2,vpa3) = mkRule p' in AltExpr
          ([vpa1],parse_apply vpa2, sem_apply f vpa3)

join_seq :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) =>
              CombinatorOptions -> i t (a -> b) -> s t a -> AltExpr t b
join_seq local_opts pl' pr' =
  let AltExpr (vimp1,vimp2,vimp3) = toAlt pl'
      SymbExpr (vpa1,vpa2,vpa3)  = mkRule pr' in AltExpr
  (vimp1++[vpa1], parse_seq vimp2 vpa2, sem_seq local_opts vimp3 vpa3)

-- join_lexical :: Nt -> RawParser t -> SymbExpr t [t]
-- join_lexical nt regex = SymbExpr (Nt nt, parse_lexical nt regex, sem_slice regex)

-- join_andNot :: (Show t) => SymbExpr t a -> SymbExpr t b -> SymbExpr t a
-- join_andNot (SymbExpr (_,p_parser,p_sem)) (SymbExpr (_, q_parser, q_sem)) =
--   SymbExpr (s, parser, p_sem)
--   where parser@(s, _) = andNot p_parser q_parser

-- |
-- Class for lifting to 'SymbExpr'.
class IsSymbExpr a where
    toSymb :: (Parseable t, Show t, Ord t) => a t b -> SymbExpr t b
    -- | Synonym of 'toSymb' for creating /derived combinators/.
    mkRule :: (Parseable t, Show t, Ord t) => a t b -> BNF t b
    mkRule = toSymb

instance IsSymbExpr AltExpr where
    toSymb = toSymb . OO . (:[])

instance IsSymbExpr SymbExpr where
    toSymb = id

instance IsSymbExpr AltExprs where
    toSymb a = mkNtRule False False mkName a
        where mkName = "_" ++ "(" ++ intercalate "|" (map op (unOO a)) ++ ")"
                where op (AltExpr (rhs,_,_)) = "(" ++ intercalate "*" (map show rhs) ++ ")"


-- |
-- Class for lifting to 'AltExprs'.
class HasAlts a where
    altsOf :: (Parseable t, Show t, Ord t) => a t b -> [AltExpr t b]

instance HasAlts AltExpr where
    altsOf = (:[])

instance HasAlts SymbExpr where
    altsOf = altsOf . toAlt

instance HasAlts AltExprs where
    altsOf = unOO

-- |
-- Class for lifting to 'AltExpr'.
class IsAltExpr a where
    toAlt :: (Parseable t, Show t, Ord t) => a t b -> AltExpr t b

instance IsAltExpr AltExpr where
    toAlt = id

instance IsAltExpr SymbExpr where
    toAlt p = join_apply (:[]) p

instance IsAltExpr AltExprs where
    toAlt = toAlt . mkRule

