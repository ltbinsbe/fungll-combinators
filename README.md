# gll-combinators

This is the GitHub repository of the Hackage package [fungll-combinators](https://hackage.haskell.org/package/fungll-combinators) and is the successor to the [gll-combinators repository](https://github.com/ltbinsbe/gll-combinators). 

Both repositories provide the same interface (external combinators). Internally the fungll-combinators use parser combinators (as opposed to grammar combinators). The fungll-combinators are employed to develop combinator expressions that closely resemble EBNF like grammar specifications. The parsers derived from combinators expressions are generalised top-down (GLL) parsers, returning all valid interpretations of an input string as specified through semantic actions. Combinators are available to specify ambiguity reduction strategies. Documentation is available on [Hackage](https://hackage.haskell.org/package/fungll-combinators/docs/GLL-ParserCombinators.html).

The following acadamic papers describe the workings of these combinator libraries:  

* fungll-combinators: [Purely functional GLL parsing](https://doi.org/10.1016/j.cola.2020.100945)
* gll-combinators: [GLL parsing with flexible combinators](https://doi.org/10.1145/3276604.3276618)
* Pre-prints are available [here](https://ltvanbinsbergen.nl/publications)
