{-# LANGUAGE InstanceSigs, FlexibleInstances #-}

module GLL.Combinators.Applicative where

import GLL.Types.Grammar
import GLL.Combinators.Visit.Join
import GLL.Combinators.Visit.Sem

import Data.Text (pack)
import qualified Data.Map as M

instance Functor (SymbExpr Token) where
  fmap f (SymbExpr (vs1, vs2, vs3)) = SymbExpr (vs1, vs2, vs3')  
    where vs3' x1 x2 x3 x4 x5 x6 = vs3 x1 x2 x3 x4 x5 x6 >>= return . fmap f

instance Applicative (SymbExpr Token) where
  pure a = SymbExpr (Nt x, M.insert x [Prod x []], vs3) 
    where x = pack "__eps"
          vs3 _ _ _ _ _ _ = return [a]
    {-  WOULD BREAK identity LAW
          vs3 _ _ _ _ l r | l == r = return [a]
                          | otherwise = return []
    -}
                        
  -- requires (Show t, Ord t) =>           
  (<*>) :: SymbExpr Token (a -> b) -> SymbExpr Token a -> SymbExpr Token b 
  p <*> q = mkRule (join_seq [] p q)

