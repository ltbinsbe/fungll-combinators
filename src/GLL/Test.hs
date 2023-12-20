{-#LANGUAGE OverloadedStrings#-}
import GLL.Combinators.Visit.FUNGLL
import GLL.Parseable.Char()
import GLL.Types.Input
import Data.Text
import GLL.Types.Grammar
import Data.Set as S
import GLL.Combinators.Interface

nt :: Text
nt = "X"

nt2 :: Text
nt2 = "Z"

choice = ((altStart `altOp` alternative1) `altOp` alternative2) `altOp` seqStart

symb = nterm nt choice -- (altStart `altOp` (parse_apply symb))

alternative1 = (seqStart `seqOp` parse_term 'a') `seqOp` symb

alternative2 = (seqStart `seqOp` parse_term 'b')

pfor = parser_for nt symb (mkInput "aab")



pz = nterm nt pzch

pzch = (altStart `altOp` zseq) `altOp` seqStart

zseq = seqStart `seqOp` parse_term 'z'



px = nterm nt2 pxch

pxch = (altStart `altOp` xseq)

xseq = (seqStart `seqOp` pz) `seqOp` parse_term '1'


pforz = parser_for nt2 px (mkInput "1")



pz2 = nterm nt pzch2

pzch2 = (altStart `altOp` zseq2) `altOp` seqStart

zseq2 = seqStart `seqOp` parse_term 'z'



px2 = nterm nt2 pxch2

pxch2 = (altStart `altOp` xseq20) `altOp` xseq21

xseq20 = seqStart `seqOp` pz2

xseq21 = seqStart `seqOp` parse_term '1'

pforz2 = parser_for nt2 px2 (mkInput "1")

pforz21 = parser_for nt2 px2 (mkInput "")

pforz22 = parser_for nt2 px2 (mkInput "z")



-- More

ntMore :: Text
ntMore = "More"

altops = (altStart `altOp` seqStart) `altOp` seq2

seq2 = ((seqStart `seqOp` parse_term ',') `seqOp` parse_term 'a') `seqOp` more

more = nterm ntMore altops

moreinput = parser_for ntMore more (mkInput ",a")


-- pX :: BNF Char Int
-- pX = "X" <::=> 1 <$$ char '1' <||> sum <$$> multipleSepBy pX (char ';')
