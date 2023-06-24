cabal:
	cabal install --force-reinstalls

interface:
	ghci src/GLL/Combinators/Interface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances

bininterface:
	ghci src/GLL/Combinators/BinaryInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances

geninterface_uuagc:
	uuagc -fHsw --noperruletypesigs --noperruletypesigs --newtypes --module=GLL.Combinators.GenInterface --output=dist/build/GenInterface.hs src/GLL/Combinators/GenInterface.ag

geninterface: geninterface_uuagc
	ghci dist/build/GenInterface.hs -XScopedTypeVariables -XTypeSynonymInstances -isrc:dist/build

runtests: cabal 
	echo "main" | ghci src/GLL/Combinators/Test/Interface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

runmemtests: cabal 
	echo "main" | ghci src/GLL/Combinators/Test/MemInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

unittest:
	ghci tests/interface/UnitTests.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

runbintests: cabal
	echo "main" | ghci src/GLL/Combinators/Test/BinaryInterface.hs -isrc:dist/build

runmembintests: cabal
	echo "main" | ghci src/GLL/Combinators/Test/MemBinInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

