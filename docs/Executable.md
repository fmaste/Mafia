# Introspecting GHC from the outside

###### An analysis of the executables and libraries that GHC produces to reverse engineer how it works.

> \- GHC is the Glasgow Haskell compiler.

> \- Glasgow, Kentucky, in the US ?

> \- No, Glasgow, Scotland, in the EU.

> \- Sure EU ? Just UK.

> \- Sure UK ?

## Example source code

###### File Main.hs
```haskell
module Main (main) where

import Foo (foo)
import Bar (bar)

main :: IO ()
main = print $ foo + bar
```
###### File Foo.hs
```haskell
module Foo (foo) where

foo :: Int
foo = 1
```
###### File Bar.hs
```haskell
module Bar (bar) where

bar :: Int
bar = 1
```

## Compiling

###### A really verbose state of the art compiler in action.

Using `-v5` as parameter, the highest verbosity level, to 
dump the output to a file.

```shellsession
fmaste@fmaste:~$ cd /tmp
fmaste@fmaste:/tmp$ mkdir hs
fmaste@fmaste:/tmp$ cd hs
fmaste@fmaste:/tmp$ printf "module Main (main) where\n\nimport Foo (foo)\nimport Bar (bar)\n\nmain :: IO ()\nmain = print $ foo + bar\n\n" "" > Main.hs
fmaste@fmaste:/tmp$ printf "module Foo (foo) where\n\nfoo :: Int\nfoo = 1\n\n" "" > Foo.hs
fmaste@fmaste:/tmp$ printf "module Bar (bar) where\n\nbar :: Int\nbar = 1\n\n" "" > Bar.hs
fmaste@fmaste:/tmp/hs$ ghc -v5 -keep-tmp-files -tmpdir . -ddump-to-file Main.hs 2> Main.stderr
[1 of 3] Compiling Bar              ( Bar.hs, Bar.o )
[2 of 3] Compiling Foo              ( Foo.hs, Foo.o )
[3 of 3] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
fmaste@fmaste:/tmp/hs$ ./Main
2
fmaste@fmaste:/tmp/hs$
```

## Log hightimes!

###### A detailed look at what we obtain.

### Info about the compiler in use.

This part below is repeated twice just changing
`wired-in package rts mapped to rts`
to 
`wired-in package rts mapped to rts-1.0`.

```shellsession
Glasgow Haskell Compiler, Version 8.0.1, stage 2 booted by GHC version 7.8.4
Using binary package database: /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/package.conf.d/package.cache
loading package database /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/package.conf.d/
wired-in package ghc-prim mapped to ghc-prim-0.5.0.0
wired-in package integer-gmp mapped to integer-gmp-1.0.0.1
wired-in package base mapped to base-4.9.0.0
wired-in package rts mapped to rts
wired-in package template-haskell mapped to template-haskell-2.11.0.0
wired-in package ghc mapped to ghc-8.0.1
wired-in package dph-seq not found.
wired-in package dph-par not found.
```

Here goes a dump, that I won't repeat, of all the "packages" that are 
available to the compiler.

What is package ? We'll try to find out.

```shellsession
loading package database /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/package.conf.d/
wired-in package ghc-prim mapped to ghc-prim-0.5.0.0
wired-in package integer-gmp mapped to integer-gmp-1.0.0.1
wired-in package base mapped to base-4.9.0.0
wired-in package rts mapped to rts-1.0
wired-in package template-haskell mapped to template-haskell-2.11.0.0
wired-in package ghc mapped to ghc-8.0.1
wired-in package dph-seq not found.
wired-in package dph-par not found.
```

### Preprocessing

Runs phase `Cpp` and `HsPp` three times,
GHC found the dependencies of `Main`
and is also processing those files.

```shellsession
*** Chasing dependencies:
Chasing modules from: *Main.hs
Running the pipeline
Running phase Cpp HsSrcFile
Running phase HsPp HsSrcFile
Running the pipeline
Running phase Cpp HsSrcFile
Running phase HsPp HsSrcFile
Running the pipeline
Running phase Cpp HsSrcFile
Running phase HsPp HsSrcFile
```

### Dependencies found

`Foo` and `Bar` import `Prelude`,
`Main` imports `Prelude` besides `Foo` and `Bar`.

It appears that the list is ordered in the same way 
that files can be easily compiled, `Main` is the last one
because it needs `Foo` and `Bar`. This last two are
ordered alphabetically even though `Foo` is imported
first and its date is earlier.

```shellsession
!!! Chasing dependencies: finished in 0.00 milliseconds, allocated 0.719 megabytes
Stable obj: []
Stable BCO: []
Ready for upsweep
  [NONREC
      ModSummary {
         ms_hs_date = 2016-08-18 21:57:47.28639993 UTC
         ms_mod = Bar,
         ms_textual_imps = [(Nothing, Prelude)]
         ms_srcimps = []
      },
   NONREC
      ModSummary {
         ms_hs_date = 2016-08-18 21:57:34.970430723 UTC
         ms_mod = Foo,
         ms_textual_imps = [(Nothing, Prelude)]
         ms_srcimps = []
      },
   NONREC
      ModSummary {
         ms_hs_date = 2016-08-18 22:00:25.362114498 UTC
         ms_mod = Main,
         ms_textual_imps = [(Nothing, Prelude), (Nothing, Bar),
                            (Nothing, Foo)]
         ms_srcimps = []
      }]
```

### Compile of `Bar.hs` to object code

- asm file: `/ghc7574_0/ghc_2.s`
- gcc: `-fno-stack-protector` `-DTABLES_NEXT_TO_CODE` `-I.` `-x assembler` `-c ./ghc7574_0/ghc_2.s` `-o ./Bar.o`

```shellsession
compiling mod: Bar
compile: input file ./Bar.hs
*** Checking old interface for Bar:
*** Parser [Bar]:
!!! Parser [Bar]: finished in 0.00 milliseconds, allocated 0.308 megabytes
*** Renamer/typechecker [Bar]:
!!! Renamer/typechecker [Bar]: finished in 44.00 milliseconds, allocated 13.991 megabytes
*** Desugar [Bar]:
!!! Desugar [Bar]: finished in 0.00 milliseconds, allocated 0.528 megabytes
*** Simplifier [Bar]:
Result size of Simplifier iteration=1
  = {terms: 13, types: 4, coercions: 0}
Result size of Simplifier = {terms: 13, types: 4, coercions: 0}
!!! Simplifier [Bar]: finished in 0.00 milliseconds, allocated 0.864 megabytes
*** CoreTidy [Bar]:
!!! CoreTidy [Bar]: finished in 4.00 milliseconds, allocated 3.011 megabytes
writeBinIface: 1 Names
writeBinIface: 18 dict entries
Created temporary directory: ./ghc7574_0
Running the pipeline
Running phase HscOut
*** CorePrep [Bar]:
!!! CorePrep [Bar]: finished in 4.00 milliseconds, allocated 0.457 megabytes
*** Stg2Stg:
*** CodeGen [Bar]:
Outputing asm to ./ghc7574_0/ghc_2.s
!!! CodeGen [Bar]: finished in 8.00 milliseconds, allocated 9.583 megabytes
Running phase As False
Running the assembler
*** Assembler:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -I. -x assembler -c ./ghc7574_0/ghc_2.s -o ./Bar.o
```

### Compile of `Foo.hs` to object code

- asm file: `/ghc7574_0/ghc_5.s`
- gcc: `-fno-stack-protector` `-DTABLES_NEXT_TO_CODE` `-I.` `-x assembler` `-c ./ghc7574_0/ghc_5.s` `-o ./Foo.o`

```shellsession
compiling mod: Foo
compile: input file ./Foo.hs
*** Checking old interface for Foo:
*** Parser [Foo]:
!!! Parser [Foo]: finished in 0.00 milliseconds, allocated 0.268 megabytes
*** Renamer/typechecker [Foo]:
!!! Renamer/typechecker [Foo]: finished in 0.00 milliseconds, allocated 0.324 megabytes
*** Desugar [Foo]:
!!! Desugar [Foo]: finished in 0.00 milliseconds, allocated 0.523 megabytes
*** Simplifier [Foo]:
Result size of Simplifier iteration=1
  = {terms: 13, types: 4, coercions: 0}
Result size of Simplifier = {terms: 13, types: 4, coercions: 0}
!!! Simplifier [Foo]: finished in 0.00 milliseconds, allocated 0.858 megabytes
*** CoreTidy [Foo]:
!!! CoreTidy [Foo]: finished in 0.00 milliseconds, allocated 0.441 megabytes
writeBinIface: 1 Names
writeBinIface: 18 dict entries
Running the pipeline
Running phase HscOut
*** CorePrep [Foo]:
!!! CorePrep [Foo]: finished in 0.00 milliseconds, allocated 0.456 megabytes
*** Stg2Stg:
*** CodeGen [Foo]:
Outputing asm to ./ghc7574_0/ghc_5.s
!!! CodeGen [Foo]: finished in 8.00 milliseconds, allocated 9.540 megabytes
Running phase As False
Running the assembler
*** Assembler:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -I. -x assembler -c ./ghc7574_0/ghc_5.s -o ./Foo.o
```

### Compile of `Main.hs` to object code

- asm file: `/ghc7574_0/ghc_8.s`
- gcc: `-fno-stack-protector` `-DTABLES_NEXT_TO_CODE` `-I.` `-x assembler` `-c ./ghc7574_0/ghc_8.s` `-o ./Main.o`

```shellsession
compiling mod: Main
compile: input file Main.hs
*** Checking old interface for Main:
*** Parser [Main]:
!!! Parser [Main]: finished in 4.00 milliseconds, allocated 0.399 megabytes
*** Renamer/typechecker [Main]:
!!! Renamer/typechecker [Main]: finished in 8.00 milliseconds, allocated 8.835 megabytes
*** Desugar [Main]:
!!! Desugar [Main]: finished in 0.00 milliseconds, allocated 1.057 megabytes
*** Simplifier [Main]:
Result size of Simplifier iteration=1
  = {terms: 20, types: 10, coercions: 0}
Result size of Simplifier = {terms: 20, types: 10, coercions: 0}
!!! Simplifier [Main]: finished in 4.00 milliseconds, allocated 1.299 megabytes
*** CoreTidy [Main]:
!!! CoreTidy [Main]: finished in 0.00 milliseconds, allocated 0.571 megabytes
writeBinIface: 1 Names
writeBinIface: 24 dict entries
Running the pipeline
Running phase HscOut
*** CorePrep [Main]:
!!! CorePrep [Main]: finished in 0.00 milliseconds, allocated 0.669 megabytes
*** Stg2Stg:
*** CodeGen [Main]:
Outputing asm to ./ghc7574_0/ghc_8.s
!!! CodeGen [Main]: finished in 24.00 milliseconds, allocated 40.096 megabytes
Running phase As False
Running the assembler
*** Assembler:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -I. -x assembler -c ./ghc7574_0/ghc_8.s -o Main.o
```

### Upsweep?

From https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Driver
> Dependency analysis inside GHC is often referred to as downsweep. 

```shellsession
Upsweep completely successful.
```

### Link

```shellsession
link: linkables are ...
LinkableM (2016-08-18 22:01:04.470071346 UTC) Main
   [DotO Main.o]
LinkableM (2016-08-18 22:01:04.42607139 UTC) Foo
   [DotO ./Foo.o]
LinkableM (2016-08-18 22:01:04.406071411 UTC) Bar
   [DotO ./Bar.o]
```

Here below we see two calls to `gcc` to compile files `ghc_10.c`
and `ghc_13.s` There's no mention of this two files anywhere else
because they are autogenerated by GHC.

Also includes files in
`/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/include`

```shellsession
*** C Compiler:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -c ./ghc7574_0/ghc_10.c -o ./ghc7574_0/ghc_11.o -I/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/include
*** C Compiler:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -c ./ghc7574_0/ghc_13.s -o ./ghc7574_0/ghc_14.o
```

#### Content of `ghc_10.c` is the call to start the RTS:

```c
#include "Rts.h"
extern StgClosure ZCMain_main_closure;
int main(int argc, char *argv[])
{
 RtsConfig __conf = defaultRtsConfig;
 __conf.rts_opts_enabled = RtsOptsSafeOnly;
 __conf.rts_opts_suggestions = rtsTrue;
 __conf.rts_hs_main = rtsTrue;
 return hs_main(argc,argv,&ZCMain_main_closure,__conf);
}
```

Files in `/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/include`

```shellsession
fmaste@fmaste:/tmp/hs$ ls /opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/include
Cmm.h         DerivedConstants.h   ffi.h          ffitarget.h   ghcautoconf.h
ghcconfig.h   ghcplatform.h        ghcversion.h   HsFFI.h       MachDeps.h
rts/          RtsAPI.h             Rts.h          stg/          Stg.h
```

#### Content of assembly file `ghc_13.s`:

```assembly
.section .debug-ghc-link-info,"",@note
.int 14
.int 2602
.int 0
.asciz "GHC link info"
.align 4
.ascii "(
```

This string that starts above contains Haskell code,
we will show it in Haskell syntax!

```haskell
          (
                  [
                          "-lHSghc-8.0.1"
                        , "-lHSprocess-1.4.2.0"
                        , "-lHShpc-0.6.0.3"
                        , "-lHShoopl-3.10.2.1"
                        , "-lHSghci-8.0.1"
                        , "-lHStransformers-0.5.2.0"
                        , "-lHStemplate-haskell-2.11.0.0"
                        , "-lHSpretty-1.1.3.3"
                        , "-lHSghc-boot-8.0.1"
                        , "-lHSghc-boot-th-8.0.1"
                        , "-lHSdirectory-1.2.6.2"
                        , "-lHSunix-2.7.2.0"
                        , "-lHStime-1.6.0.1"
                        , "-lHSfilepath-1.4.1.0"
                        , "-lHSbinary-0.8.3.0"
                        , "-lHScontainers-0.5.7.1"
                        , "-lHSbytestring-0.10.8.1"
                        , "-lHSdeepseq-1.4.2.0"
                        , "-lHSarray-0.5.1.1"
                        , "-lHSbase-4.9.0.0"
                        , "-lHSinteger-gmp-1.0.0.1"
                        , "-lHSghc-prim-0.5.0.0"
                        , "-lHSrts"
                        , "-lCffi"
                ]
                , [
                          "-lrt"
                        , "-lutil"
                        , "-ldl"
                        , "-lpthread"
                        , "-lgmp"
                        , "-lm"
                        , "-lrt"
                        , "-ldl"
                ]
                , [
                          "-Wl,-u,ghczmprim_GHCziTypes_Izh_static_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Czh_static_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Fzh_static_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Dzh_static_info"
                        , "-Wl,-u,base_GHCziPtr_Ptr_static_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Wzh_static_info"
                        , "-Wl,-u,base_GHCziInt_I8zh_static_info"
                        , "-Wl,-u,base_GHCziInt_I16zh_static_info"
                        , "-Wl,-u,base_GHCziInt_I32zh_static_info"
                        , "-Wl,-u,base_GHCziInt_I64zh_static_info"
                        , "-Wl,-u,base_GHCziWord_W8zh_static_info"
                        , "-Wl,-u,base_GHCziWord_W16zh_static_info"
                        , "-Wl,-u,base_GHCziWord_W32zh_static_info"
                        , "-Wl,-u,base_GHCziWord_W64zh_static_info"
                        , "-Wl,-u,base_GHCziStable_StablePtr_static_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Izh_con_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Czh_con_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info"
                        , "-Wl,-u,base_GHCziPtr_Ptr_con_info"
                        , "-Wl,-u,base_GHCziPtr_FunPtr_con_info"
                        , "-Wl,-u,base_GHCziStable_StablePtr_con_info"
                        , "-Wl,-u,ghczmprim_GHCziTypes_False_closure"
                        , "-Wl,-u,ghczmprim_GHCziTypes_True_closure"
                        , "-Wl,-u,base_GHCziPack_unpackCString_closure"
                        , "-Wl,-u,base_GHCziIOziException_stackOverflow_closure"
                        , "-Wl,-u,base_GHCziIOziException_heapOverflow_closure"
                        , "-Wl,-u,base_ControlziExceptionziBase_nonTermination_closure"
                        , "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure"
                        , "-Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure"
                        , "-Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure"
                        , "-Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure"
                        , "-Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure"
                        , "-Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure"
                        , "-Wl,-u,base_GHCziTopHandler_flushStdHandles_closure"
                        , "-Wl,-u,base_GHCziTopHandler_runIO_closure"
                        , "-Wl,-u,base_GHCziTopHandler_runNonIO_closure"
                        , "-Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure"
                        , "-Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure"
                        , "-Wl,-u,base_GHCziConcziSync_runSparks_closure"
                        , "-Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure"
                ]
        )
        , []
        , Nothing
        , RtsOptsSafeOnly
        , False
        , []
        , []
```

Nice Haskell in assembly hack, now finish the assembly code

```assembly
)"
.align 4
```

#### Note on `compiler/main/DriverPipeline.hs`:

> The "link info" is a string representing the parameters of the link. We save
> this information in the binary, and the next time we link, if nothing else has
> changed, we use the link info stored in the existing binary to decide whether
> to re-link or not.
> The "link info" string is stored in a ELF section called
> ".debug-ghc-link-info" (see ghcLinkInfoSectionName) with the SHT_NOTE type.
> For some time, it used to not follow the specified record-based format (see
> 11022).

```shellsession
*** Linker:
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE '-Wl,--hash-size=31' -Wl,--reduce-memory-overheads -Wl,--no-as-needed -v -o Main -Wl,--gc-sections Main.o ./Foo.o ./Bar.o -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/process-1.4.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hpc-0.6.0.3 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hoopl-3.10.2.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghci-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/transformers-0.5.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/template-haskell-2.11.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/pretty-1.1.3.3 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-th-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/directory-1.2.6.2 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/unix-2.7.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/time-1.6.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/filepath-1.4.1.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/binary-0.8.3.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/containers-0.5.7.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/bytestring-0.10.8.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/deepseq-1.4.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/array-0.5.1.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/base-4.9.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/integer-gmp-1.0.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-prim-0.5.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/rts ./ghc7574_0/ghc_11.o ./ghc7574_0/ghc_14.o -Wl,-u,ghczmprim_GHCziTypes_Izh_static_info -Wl,-u,ghczmprim_GHCziTypes_Czh_static_info -Wl,-u,ghczmprim_GHCziTypes_Fzh_static_info -Wl,-u,ghczmprim_GHCziTypes_Dzh_static_info -Wl,-u,base_GHCziPtr_Ptr_static_info -Wl,-u,ghczmprim_GHCziTypes_Wzh_static_info -Wl,-u,base_GHCziInt_I8zh_static_info -Wl,-u,base_GHCziInt_I16zh_static_info -Wl,-u,base_GHCziInt_I32zh_static_info -Wl,-u,base_GHCziInt_I64zh_static_info -Wl,-u,base_GHCziWord_W8zh_static_info -Wl,-u,base_GHCziWord_W16zh_static_info -Wl,-u,base_GHCziWord_W32zh_static_info -Wl,-u,base_GHCziWord_W64zh_static_info -Wl,-u,base_GHCziStable_StablePtr_static_info -Wl,-u,ghczmprim_GHCziTypes_Izh_con_info -Wl,-u,ghczmprim_GHCziTypes_Czh_con_info -Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info -Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info -Wl,-u,base_GHCziPtr_Ptr_con_info -Wl,-u,base_GHCziPtr_FunPtr_con_info -Wl,-u,base_GHCziStable_StablePtr_con_info -Wl,-u,ghczmprim_GHCziTypes_False_closure -Wl,-u,ghczmprim_GHCziTypes_True_closure -Wl,-u,base_GHCziPack_unpackCString_closure -Wl,-u,base_GHCziIOziException_stackOverflow_closure -Wl,-u,base_GHCziIOziException_heapOverflow_closure -Wl,-u,base_ControlziExceptionziBase_nonTermination_closure -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure -Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure -Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure -Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure -Wl,-u,base_GHCziTopHandler_flushStdHandles_closure -Wl,-u,base_GHCziTopHandler_runIO_closure -Wl,-u,base_GHCziTopHandler_runNonIO_closure -Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure -Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure -Wl,-u,base_GHCziConcziSync_runSparks_closure -Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure -lHSghc-8.0.1 -lHSprocess-1.4.2.0 -lHShpc-0.6.0.3 -lHShoopl-3.10.2.1 -lHSghci-8.0.1 -lHStransformers-0.5.2.0 -lHStemplate-haskell-2.11.0.0 -lHSpretty-1.1.3.3 -lHSghc-boot-8.0.1 -lHSghc-boot-th-8.0.1 -lHSdirectory-1.2.6.2 -lHSunix-2.7.2.0 -lHStime-1.6.0.1 -lHSfilepath-1.4.1.0 -lHSbinary-0.8.3.0 -lHScontainers-0.5.7.1 -lHSbytestring-0.10.8.1 -lHSdeepseq-1.4.2.0 -lHSarray-0.5.1.1 -lHSbase-4.9.0.0 -lHSinteger-gmp-1.0.0.1 -lHSghc-prim-0.5.0.0 -lHSrts -lCffi -lrt -lutil -ldl -lpthread -lgmp -lm -lrt -ldl
```

All this -Wl options come from ld-options of the rts packet.

From `gcc` man: 

```
-Wl,option:
   Pass option as an option to the linker.
```

```
-Ldir
   Add directory dir to the list of directories to be searched for -l.
```

```
-l library
   Search the library named library when linking.

   It makes a difference where in the command you write this option; the
   linker searches and processes libraries and object files in the order
   they are specified. Thus, foo.o -lz bar.o searches library z after
   file foo.o but before bar.o. If bar.o refers to functions in z, those
   functions may not be loaded.
   The linker searches a standard list of directories for the library,
   which is actually a file named liblibrary.a. The linker then uses
   this file as if it had been specified precisely by name.
   The directories searched include several standard system directories
   plus any that you specify with -L.
   Normally the files found this way are library files---archive files
   whose members are object files. The linker handles an archive file by
   scanning through it for members which define symbols that have so far
   been referenced but not defined. But if the file that is found is an
   ordinary object file, it is linked in the usual fashion. The only
   difference between using an -l option and specifying a file name is
   that -l surrounds library with lib and .a and searches several
   directories.
```

From `ld` man: 

```
-u symbol
--undefined=symbol
   Force symbol to be entered in the output file as an undefined symbol.
   Doing this may, for example, trigger linking of additional modules
   from standard libraries.
```

`/usr/bin/gcc`

*        -fno-stack-protector
*        -DTABLES_NEXT_TO_CODE
*        '-Wl,--hash-size=31'
*        -Wl,--reduce-memory-overheads
*        -Wl,--no-as-needed
*        -v
*        -o Main
*        -Wl,--gc-sections
*        Main.o
*        ./Foo.o
*        ./Bar.o
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-8.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/process-1.4.2.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hpc-0.6.0.3
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hoopl-3.10.2.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghci-8.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/transformers-0.5.2.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/template-haskell-2.11.0.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/pretty-1.1.3.3
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-8.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-th-8.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/directory-1.2.6.2
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/unix-2.7.2.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/time-1.6.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/filepath-1.4.1.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/binary-0.8.3.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/containers-0.5.7.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/bytestring-0.10.8.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/deepseq-1.4.2.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/array-0.5.1.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/base-4.9.0.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/integer-gmp-1.0.0.1
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-prim-0.5.0.0
*        -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/rts
*        ./ghc7574_0/ghc_11.o
*        ./ghc7574_0/ghc_14.o
*        -Wl,-u,ghczmprim_GHCziTypes_Izh_static_info
*        -Wl,-u,ghczmprim_GHCziTypes_Czh_static_info
*        -Wl,-u,ghczmprim_GHCziTypes_Fzh_static_info
*        -Wl,-u,ghczmprim_GHCziTypes_Dzh_static_info
*        -Wl,-u,base_GHCziPtr_Ptr_static_info
*        -Wl,-u,ghczmprim_GHCziTypes_Wzh_static_info
*        -Wl,-u,base_GHCziInt_I8zh_static_info
*        -Wl,-u,base_GHCziInt_I16zh_static_info
*        -Wl,-u,base_GHCziInt_I32zh_static_info
*        -Wl,-u,base_GHCziInt_I64zh_static_info
*        -Wl,-u,base_GHCziWord_W8zh_static_info
*        -Wl,-u,base_GHCziWord_W16zh_static_info
*        -Wl,-u,base_GHCziWord_W32zh_static_info
*        -Wl,-u,base_GHCziWord_W64zh_static_info
*        -Wl,-u,base_GHCziStable_StablePtr_static_info
*        -Wl,-u,ghczmprim_GHCziTypes_Izh_con_info
*        -Wl,-u,ghczmprim_GHCziTypes_Czh_con_info
*        -Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info
*        -Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info
*        -Wl,-u,base_GHCziPtr_Ptr_con_info
*        -Wl,-u,base_GHCziPtr_FunPtr_con_info
*        -Wl,-u,base_GHCziStable_StablePtr_con_info
*        -Wl,-u,ghczmprim_GHCziTypes_False_closure
*        -Wl,-u,ghczmprim_GHCziTypes_True_closure
*        -Wl,-u,base_GHCziPack_unpackCString_closure
*        -Wl,-u,base_GHCziIOziException_stackOverflow_closure
*        -Wl,-u,base_GHCziIOziException_heapOverflow_closure
*        -Wl,-u,base_ControlziExceptionziBase_nonTermination_closure
*        -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure
*        -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure
*        -Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure
*        -Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure
*        -Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure
*        -Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure
*        -Wl,-u,base_GHCziTopHandler_flushStdHandles_closure
*        -Wl,-u,base_GHCziTopHandler_runIO_closure
*        -Wl,-u,base_GHCziTopHandler_runNonIO_closure
*        -Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure
*        -Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure
*        -Wl,-u,base_GHCziConcziSync_runSparks_closure
*        -Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure
*        -lHSghc-8.0.1
*        -lHSprocess-1.4.2.0
*        -lHShpc-0.6.0.3
*        -lHShoopl-3.10.2.1
*        -lHSghci-8.0.1
*        -lHStransformers-0.5.2.0
*        -lHStemplate-haskell-2.11.0.0
*        -lHSpretty-1.1.3.3
*        -lHSghc-boot-8.0.1
*        -lHSghc-boot-th-8.0.1
*        -lHSdirectory-1.2.6.2
*        -lHSunix-2.7.2.0
*        -lHStime-1.6.0.1
*        -lHSfilepath-1.4.1.0
*        -lHSbinary-0.8.3.0
*        -lHScontainers-0.5.7.1
*        -lHSbytestring-0.10.8.1
*        -lHSdeepseq-1.4.2.0
*        -lHSarray-0.5.1.1
*        -lHSbase-4.9.0.0
*        -lHSinteger-gmp-1.0.0.1
*        -lHSghc-prim-0.5.0.0
*        -lHSrts
*        -lCffi
*        -lrt
*        -lutil
*        -ldl
*        -lpthread
*        -lgmp
*        -lm
*        -lrt
*        -ldl

## Collect

> GCC uses a utility called collect2 on nearly all systems to arrange to call various initialization functions at start time. 

https://gcc.gnu.org/onlinedocs/gccint/Collect2.html#Collect2

```shellsession
Using built-in specs.
COLLECT_GCC=/usr/bin/gcc
COLLECT_LTO_WRAPPER=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper
Target: x86_64-linux-gnu
Configured with: ../src/configure -v --with-pkgversion='Ubuntu 5.4.0-6ubuntu1~16.04.2' --with-bugurl=file:///usr/share/doc/gcc-5/README.Bugs --enable-languages=c,ada,c++,java,go,d,fortran,objc,obj-c++ --prefix=/usr --program-suffix=-5 --enable-shared --enable-linker-build-id --libexecdir=/usr/lib --without-included-gettext --enable-threads=posix --libdir=/usr/lib --enable-nls --with-sysroot=/ --enable-clocale=gnu --enable-libstdcxx-debug --enable-libstdcxx-time=yes --with-default-libstdcxx-abi=new --enable-gnu-unique-object --disable-vtable-verify --enable-libmpx --enable-plugin --with-system-zlib --disable-browser-plugin --enable-java-awt=gtk --enable-gtk-cairo --with-java-home=/usr/lib/jvm/java-1.5.0-gcj-5-amd64/jre --enable-java-home --with-jvm-root-dir=/usr/lib/jvm/java-1.5.0-gcj-5-amd64 --with-jvm-jar-dir=/usr/lib/jvm-exports/java-1.5.0-gcj-5-amd64 --with-arch-directory=amd64 --with-ecj-jar=/usr/share/java/eclipse-ecj.jar --enable-objc-gc --enable-multiarch --disable-werror --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --with-tune=generic --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
Thread model: posix
gcc version 5.4.0 20160609 (Ubuntu 5.4.0-6ubuntu1~16.04.2)
COMPILER_PATH=/usr/lib/gcc/x86_64-linux-gnu/5/:/usr/lib/gcc/x86_64-linux-gnu/5/:/usr/lib/gcc/x86_64-linux-gnu/:/usr/lib/gcc/x86_64-linux-gnu/5/:/usr/lib/gcc/x86_64-linux-gnu/
LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/5/:/usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/:/usr/lib/gcc/x86_64-linux-gnu/5/../../../../lib/:/lib/x86_64-linux-gnu/:/lib/../lib/:/usr/lib/x86_64-linux-gnu/:/usr/lib/../lib/:/usr/lib/gcc/x86_64-linux-gnu/5/../../../:/lib/:/usr/lib/
COLLECT_GCC_OPTIONS='-fno-stack-protector' '-D' 'TABLES_NEXT_TO_CODE' '-v' '-o' 'Main' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-8.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/process-1.4.2.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hpc-0.6.0.3' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hoopl-3.10.2.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghci-8.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/transformers-0.5.2.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/template-haskell-2.11.0.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/pretty-1.1.3.3' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-8.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-th-8.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/directory-1.2.6.2' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/unix-2.7.2.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/time-1.6.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/filepath-1.4.1.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/binary-0.8.3.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/containers-0.5.7.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/bytestring-0.10.8.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/deepseq-1.4.2.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/array-0.5.1.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/base-4.9.0.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/integer-gmp-1.0.0.1' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-prim-0.5.0.0' '-L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/rts' '-mtune=generic' '-march=x86-64'
 /usr/lib/gcc/x86_64-linux-gnu/5/collect2 -plugin /usr/lib/gcc/x86_64-linux-gnu/5/liblto_plugin.so -plugin-opt=/usr/lib/gcc/x86_64-linux-gnu/5/lto-wrapper -plugin-opt=-fresolution=/tmp/ccofXFxY.res -plugin-opt=-pass-through=-lgcc -plugin-opt=-pass-through=-lgcc_s -plugin-opt=-pass-through=-lc -plugin-opt=-pass-through=-lgcc -plugin-opt=-pass-through=-lgcc_s --sysroot=/ --build-id --eh-frame-hdr -m elf_x86_64 --hash-style=gnu --as-needed -dynamic-linker /lib64/ld-linux-x86-64.so.2 -z relro -o Main /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crt1.o /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crti.o /usr/lib/gcc/x86_64-linux-gnu/5/crtbegin.o -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/process-1.4.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hpc-0.6.0.3 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/hoopl-3.10.2.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghci-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/transformers-0.5.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/template-haskell-2.11.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/pretty-1.1.3.3 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-boot-th-8.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/directory-1.2.6.2 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/unix-2.7.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/time-1.6.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/filepath-1.4.1.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/binary-0.8.3.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/containers-0.5.7.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/bytestring-0.10.8.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/deepseq-1.4.2.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/array-0.5.1.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/base-4.9.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/integer-gmp-1.0.0.1 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/ghc-prim-0.5.0.0 -L/opt/haskell/ghc/8.0.1/install/lib/ghc-8.0.1/rts -L/usr/lib/gcc/x86_64-linux-gnu/5 -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu -L/usr/lib/gcc/x86_64-linux-gnu/5/../../../../lib -L/lib/x86_64-linux-gnu -L/lib/../lib -L/usr/lib/x86_64-linux-gnu -L/usr/lib/../lib -L/usr/lib/gcc/x86_64-linux-gnu/5/../../.. @/tmp/ccQbTvad -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/lib/gcc/x86_64-linux-gnu/5/crtend.o /usr/lib/gcc/x86_64-linux-gnu/5/../../../x86_64-linux-gnu/crtn.o
link: done
```

### Output files

As we compiled with `-keep-tmp-files` all the temporary files
(like the autogenerated ones we see above) aren't deleted when
the compiler finishes and with `-tmpdir .` this temporary
directory is created in our actual directory instead of `/tmp`.
With `-v5` we activate all the `-ddump-*` alternatives that dump 
all the internal phases of the compiler next to the module file
(if you forget to use `-ddump-to-file` all of this
will be on your screen).


```shellsession
fmaste@fmaste:/tmp/hs$ ls -l
Bar.dump-asm Bar.dump-asm-expanded Bar.dump-asm-liveness Bar.dump-asm-native Bar.dump-asm-regalloc
Bar.dump-cmm Bar.dump-cmm-cps Bar.dump-cmm-raw Bar.dump-debug
Bar.dump-ds
Bar.dump-foreign
Bar.dump-hi
Bar.dump-occur-anal
Bar.dump-opt-cmm
Bar.dump-parsed
Bar.dump-prep
Bar.dump-simpl Bar.dump-simpl-stats
Bar.dump-stg
Bar.hi
Bar.hs
Bar.o
Bar.source-stats
Foo.dump-asm Foo.dump-asm-expanded Foo.dump-asm-liveness Foo.dump-asm-native Foo.dump-asm-regalloc
Foo.dump-cmm Foo.dump-cmm-cps Foo.dump-cmm-raw
Foo.dump-debug
Foo.dump-ds
Foo.dump-foreign
Foo.dump-hi
Foo.dump-occur-anal
Foo.dump-opt-cmm
Foo.dump-parsed
Foo.dump-prep
Foo.dump-simpl Foo.dump-simpl-stats
Foo.dump-stg
Foo.hi
Foo.hs
Foo.o
Foo.source-stats
ghc7574_0
Main
Main.dump-asm Main.dump-asm-expanded Main.dump-asm-liveness Main.dump-asm-native Main.dump-asm-regalloc
Main.dump-cmm Main.dump-cmm-cfg Main.dump-cmm-cps Main.dump-cmm-info Main.dump-cmm-raw Main.dump-cmm-sp Main.dump-cmm-switch
Main.dump-debug
Main.dump-ds
Main.dump-foreign
Main.dump-hi
Main.dump-occur-anal
Main.dump-opt-cmm
Main.dump-parsed
Main.dump-prep
Main.dump-simpl Main.dump-simpl-stats
Main.dump-stg
Main.hi
Main.hs
Main.o
Main.source-stats
Main.txt
fmaste@fmaste:/tmp/hs$ rm Main; rm Main.o; rm Main.hi; rm Main.dump-*; rm Main.source-*;
fmaste@fmaste:/tmp/hs$ rm Foo.o; rm Foo.hi; rm Foo.dump-*; rm Foo.source-*;
fmaste@fmaste:/tmp/hs$ rm Bar.o; rm Bar.hi; rm Bar.dump-*; rm Bar.source-*;
```

### Defined symbols

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Main.o
0000000000000060 D Main_main_closure
0000000000000090 T Main_main_info
0000000000000020 D Main_zdtrModule_closure
0000000000000000 D __stginit_Main
0000000000000080 D ZCMain_main_closure
00000000000000f8 T ZCMain_main_info
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Main.o
                 U Bar_bar_closure
                 U base_GHCziNum_zdfNumInt_closure
                 U base_GHCziNum_zp_info
                 U base_GHCziShow_zdfShowInt_closure
                 U base_GHCziTopHandler_runMainIO_closure
                 U base_SystemziIO_print_closure
                 U Foo_foo_closure
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
                 U newCAF
                 U stg_ap_p_fast
                 U stg_ap_pp_fast
                 U stg_ap_pp_info
                 U stg_bh_upd_frame_info
```

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Foo.o
0000000000000000 D Foo_foo_closure
0000000000000030 D Foo_zdtrModule_closure
0000000000000000 D __stginit_Foo
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Foo.o
                 U ghczmprim_GHCziTypes_Izh_static_info
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
```

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Bar.o
0000000000000000 D Bar_bar_closure
0000000000000030 D Bar_zdtrModule_closure
0000000000000000 D __stginit_Bar
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Bar.o
                 U ghczmprim_GHCziTypes_Izh_static_info
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
```

### Defined packaged symbols

```shellsession
fmaste@fmaste:/tmp/hs$ rm Main Main.o Main.hi Foo.hi Foo.o Bar.o Bar.hi
```

Compile a package named `XXX-XXXzmXXXziXXX-XXX`

```shellsession
fmaste@fmaste:/tmp/hs$ ghc --make -this-unit-idXXX-XXXzmXXXziXXX-XXX Main.hs
[1 of 3] Compiling Bar              ( Bar.hs, Bar.o )
[2 of 3] Compiling Foo              ( Foo.hs, Foo.o )
[3 of 3] Compiling Main 
```

Main differes on the number of symbols becuase as the mindful readers
may have notice the exeutable was not created, GHC thinks we are
building a library for a package names `XXX-XXXzmXXXziXXX-XXX`

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Main.o
0000000000000000 D __stginit_XXXzmXXXzzmXXXzziXXXzmXXX_Main
0000000000000060 D XXXzmXXXzzmXXXzziXXXzmXXX_Main_main_closure
0000000000000090 T XXXzmXXXzzmXXXzziXXXzmXXX_Main_main_info
0000000000000020 D XXXzmXXXzzmXXXzziXXXzmXXX_Main_zdtrModule_closure
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Main.o
                 U base_GHCziNum_zdfNumInt_closure
                 U base_GHCziNum_zp_info
                 U base_GHCziShow_zdfShowInt_closure
                 U base_SystemziIO_print_closure
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
                 U newCAF
                 U stg_ap_pp_fast
                 U stg_ap_pp_info
                 U stg_bh_upd_frame_info
                 U XXXzmXXXzzmXXXzziXXXzmXXX_Bar_bar_closure
                 U XXXzmXXXzzmXXXzziXXXzmXXX_Foo_foo_closure
```

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Foo.o
0000000000000000 D __stginit_XXXzmXXXzzmXXXzziXXXzmXXX_Foo
0000000000000000 D XXXzmXXXzzmXXXzziXXXzmXXX_Foo_foo_closure
0000000000000030 D XXXzmXXXzzmXXXzziXXXzmXXX_Foo_zdtrModule_closure
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Foo.o
                 U ghczmprim_GHCziTypes_Izh_static_info
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
```

```shellsession
fmaste@fmaste:/tmp/hs$ nm --extern-only --defined Bar.o
0000000000000000 D __stginit_XXXzmXXXzzmXXXzziXXXzmXXX_Bar
0000000000000000 D XXXzmXXXzzmXXXzziXXXzmXXX_Bar_bar_closure
0000000000000030 D XXXzmXXXzzmXXXzziXXXzmXXX_Bar_zdtrModule_closure
fmaste@fmaste:/tmp/hs$ nm --extern-only --undefined Bar.o
                 U ghczmprim_GHCziTypes_Izh_static_info
                 U ghczmprim_GHCziTypes_Module_static_info
                 U ghczmprim_GHCziTypes_TrNameS_static_info
```

## Way to go!

```shellsession
fmaste@fmaste:/tmp/hs$ ldd Main
        linux-vdso.so.1 =>  (0x00007ffc65ffc000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f0cdc697000)
        libutil.so.1 => /lib/x86_64-linux-gnu/libutil.so.1 (0x00007f0cdc494000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f0cdc28f000)
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f0cdc072000)
        libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f0cdbdf2000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f0cdbae8000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f0cdb71f000)
        /lib64/ld-linux-x86-64.so.2 (0x00005636508eb000)
```

### Static 

```shellsession
fmaste@fmaste:/tmp/hs$ ls -la /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHS*.a
-rw-r--r-- 1 root root 3242606 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts.a
-rw-r--r-- 1 root root 2772708 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_debug.a
-rw-r--r-- 1 root root 3605500 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_l.a
-rw-r--r-- 1 root root 3827004 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_p.a
-rw-r--r-- 1 root root 4093458 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr.a
-rw-r--r-- 1 root root 3225478 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_debug.a
-rw-r--r-- 1 root root 4511180 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_l.a
-rw-r--r-- 1 root root 4715538 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_p.a
```

### Dynamic

```shellsession
fmaste@fmaste:/tmp/hs$ ls -la /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHS*.so
-rwxr-xr-x 1 root root 1275560 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_debug-ghc8.0.1.so
-rwxr-xr-x 1 root root 1513608 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts-ghc8.0.1.so
-rwxr-xr-x 1 root root 1728312 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_l-ghc8.0.1.so
-rwxr-xr-x 1 root root 1462112 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_debug-ghc8.0.1.so
-rwxr-xr-x 1 root root 1862384 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr-ghc8.0.1.so
-rwxr-xr-x 1 root root 2104480 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_l-ghc8.0.1.so
```

* Static:
   * Not threaded: Vanilla, Debug, Eventlog, Profiling
   * Threaded: Vanilla, Debug, Eventlog, Profiling
* Dynamic:
   * Not threaded: Vanilla, Debug, Eventlog
   * Threaded: Vanilla, Debug, Eventlog

```shellsession
fmaste@fmaste:/tmp/hs$ ls -la /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libffi*
-rwxr-xr-x 1 root root 60496 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libffi.so
-rw-r--r-- 1 root root 60496 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libffi.so.6
-rw-r--r-- 1 root root 60496 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libffi.so.6.0.4
```

```shellsession
fmaste@fmaste:/tmp/hs$ ls -la /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi*
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_debug.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_l.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_p.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_thr.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_thr_debug.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_thr_l.a
-rw-r--r-- 1 root root 72320 /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libCffi_thr_p.a
```

```shellsession
fmaste@fmaste:/tmp/hs$ ghc --make -threaded -debug -prof -eventlog -rtsopts Main.hs 2>&1
<command line>: combination not supported: Threaded/Debug/Profiling/RTS Event Logging
```

```shellsession
fmaste@fmaste:/tmp/hs$ ghc --make -threaded -prof -eventlog -rtsopts Main.hs 2>&1
<command line>: combination not supported: Threaded/Profiling/RTS Event Logging
```

I guess nobody wants to profile and debug at the same time!

```shellsession
fmaste@fmaste:/tmp/hs$ ghc --make -threaded -debug -prof -rtsopts Main.hs 2>&1
Linking Main ...
/usr/bin/ld: cannot find -lHSrts_thr_debug_p
collect2: error: ld returned 1 exit status
`gcc' failed in phase `Linker'. (Exit code: 1)
```

```shellsession
fmaste@fmaste:/tmp/hs$ ghc --make -threaded -debug -eventlog -rtsopts Main.hs 2>&1
[1 of 3] Compiling Bar              ( Bar.hs, Bar.o )
[2 of 3] Compiling Foo              ( Foo.hs, Foo.o )
[3 of 3] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
```

It is linked with `libHSrts_thr_debug.a` but has both eventlog and debug:

```shellsession
fmaste@fmaste:/tmp/hs$ ghc -v4 --make -threaded -debug -eventlog -rtsopts Main.hs 2>&1 | grep HSrts
/usr/bin/gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE ... -lHSrts_thr_debug ...
fmaste@fmaste:/tmp/hs$ ./Main +RTS -l +RTS -Dr
...
7fb3a26f8700: cap 0: running thread 4 (ThreadRunGHC)
7fb3a26f8700: cap 0: thread 4 stopped (suspended while making a foreign call)
2
7fb3a26f8700: cap 0: running thread 4 (ThreadRunGHC)
7fb3a26f8700: cap 0: thread 4 stopped (finished)
...
fmaste@fmaste:/tmp/hs$ cat Main.eventlog 
Create threadeteetb
...
```

The last one that gets linked with `-lHSrts_thr_p`

```shellsession
fmaste@fmaste:/tmp/hs$ rm Main.o Main.hi Main.eventlog Main.prof Main Foo.o Foo.hi Bar.o Bar.hi;
fmaste@fmaste:/tmp/hs$ ghc --make -threaded -prof -rtsopts Main.hs
[1 of 3] Compiling Bar              ( Bar.hs, Bar.o )
[2 of 3] Compiling Foo              ( Foo.hs, Foo.o )
[3 of 3] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
```

```shellsession
fmaste@fmaste:/tmp/hs$ ./Main +RTS -p
2
fmaste@fmaste:/tmp/hs$ cat Main.prof 
	Fri Aug 19 19:13 2016 Time and Allocation Profiling Report  (Final)
...
	total time  =        0.00 secs   (1 ticks @ 1000 us, 1 processor)
	total alloc =      73,040 bytes  (excludes profiling overheads)
...
```
