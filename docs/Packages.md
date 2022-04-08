A package db with only one package named bytestring much more like bytestring
I cant add this special package db because its member depends on base, deepseq,
etc that are not available yet. So you even have to be carefull on the order.

FAILS:

fmaste@fmaste:/tmp/hs2$ ghc -v5 -keep-tmp-files -tmpdir . -ddump-to-file -clear-package-db -package-db/tmp/hs2/package.conf.d -global-package-db -package-idbytestring  Main.hs 2> Main.txt
fmaste@fmaste:/tmp/hs2$ less Main.txt
Glasgow Haskell Compiler, Version 8.0.1, stage 2 booted by GHC version 7.8.4
Using binary package database: /tmp/hs2/package.conf.d/package.cache
Using binary package database: /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/package.conf.d/package.cache
loading package database /tmp/hs2/package.conf.d
package bytestring is unusable due to missing or recursive dependencies:
  base-4.9.0.0 deepseq-1.4.2.0 ghc-prim-0.5.0.0 integer-gmp-1.0.0.1
package base-4.9.0.1 is unusable due to missing or recursive dependencies:
  ghc-prim-0.5.0.0 integer-gmp-1.0.0.1 rts
loading package database /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/package.conf.d
<command line>: cannot satisfy -package-id bytestring: 
    bytestring is unusable due to missing or recursive dependencies:
      base-4.9.0.0 deepseq-1.4.2.0 ghc-prim-0.5.0.0 integer-gmp-1.0.0.1
    (use -v for more information)


FAILS:

fmaste@fmaste:/tmp/hs2$ ghc -v5 -keep-tmp-files -tmpdir . -ddump-to-file -no-global-package-db -package-db/tmp/hs2/package.conf.d -global-package-db -package-idbytestring  Main.hs 2> Main.txt
fmaste@fmaste:/tmp/hs2$ less Main.txt 
Glasgow Haskell Compiler, Version 8.0.1, stage 2 booted by GHC version 7.8.4
Using binary package database: /home/fmaste/.ghc/x86_64-linux-8.0.1/package.conf.d/package.cache
Using binary package database: /tmp/hs2/package.conf.d/package.cache
Using binary package database: /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/package.conf.d/package.cache
loading package database /home/fmaste/.ghc/x86_64-linux-8.0.1/package.conf.d
package mtl-2.2.1-6qsR1PHUy5lL47Hpoa4jCM is unusable due to missing or recursive dependencies:
  base-4.9.0.0 transformers-0.5.2.0
loading package database /tmp/hs2/package.conf.d
package bytestring is unusable due to missing or recursive dependencies:
  base-4.9.0.0 deepseq-1.4.2.0 ghc-prim-0.5.0.0 integer-gmp-1.0.0.1
package base-4.9.0.1 is unusable due to missing or recursive dependencies:
  ghc-prim-0.5.0.0 integer-gmp-1.0.0.1 rts
loading package database /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/package.conf.d
<command line>: cannot satisfy -package-id bytestring: 
    bytestring is unusable due to missing or recursive dependencies:
      base-4.9.0.0 deepseq-1.4.2.0 ghc-prim-0.5.0.0 integer-gmp-1.0.0.1
    (use -v for more information)


WORKS:

fmaste@fmaste:/tmp/hs2$ ghc -v5 -keep-tmp-files -tmpdir . -ddump-to-file -global-package-db -package-db/tmp/hs2/package.conf.d -package-idbytestring  Main.hs 2> Main.txt
[1 of 3] Skipping  Bar              ( Bar.hs, Bar.o )
[2 of 3] Skipping  Foo              ( Foo.hs, Foo.o )
[3 of 3] Skipping  Main             ( Main.hs, Main.o )
Linking Main ...
