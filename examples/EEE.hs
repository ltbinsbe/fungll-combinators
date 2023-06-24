
import GLL.Combinators
import GLL.Parseable.Char

pE = "E" <::=>   (\x y z -> x + y + z) <$$> pE <**> pE <**> pE
            <||> 1 <$$ char '1'
            <||> satisfy 0
