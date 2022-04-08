# Mafia
A framework for packaging Haskell software

Trying to create an alternative to Cabal as a playground and research project on GHC's packages and GHC's runtime system

Due to the limitations of Cabal, the library, at that time some packages could not be built.

Usage (that used to work):
```
mafia hackage fetch array 0.5.1.1
mafia hackage install array 0.5.1.1
mafia cabal library array 0.5.1.1

mafia hackage fetch deepseq 1.4.2.0
mafia hackage install deepseq 1.4.2.0
mafia cabal library deepseq 1.4.2.0

mafia hackage fetch containers 0.5.7.1
mafia hackage install containers 0.5.7.1
mafia cabal library containers 0.5.7.1

mafia hackage fetch bytestring 0.10.8.1
mafia hackage install bytestring 0.10.8.1
mafia cabal library bytestring 0.10.8.1

mafia hackage fetch binary 0.8.4.1
mafia hackage install binary 0.8.4.1
mafia cabal library binary 0.8.4.1

mafia hackage fetch text 1.2.2.1
mafia hackage install text 1.2.2.1
mafia cabal library text 1.2.2.1
```
