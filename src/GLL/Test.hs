{-#LANGUAGE OverloadedStrings#-}
import GLL.Combinators.Visit.FUNGLL
import GLL.Parseable.Char()
import GLL.Types.Input
import Data.Text

choice = (altStart `altOp` alternative1) `altOp` alternative2

nt :: Text
nt = "X"

symb = nterm nt (altStart `altOp` (parse_apply symb))

alternative1 = (seqStart `seqOp` parse_term 'a') `seqOp` symb

alternative2 = (seqStart `seqOp` parse_term 'b')

