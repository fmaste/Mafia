# GHC 8.0.1 usage by examples

## Haskell code with no `Main` module

Example file `Foo.hs`
```haskell
module Foo (foo) where

foo :: IO ()
foo = print 'a'
```
### Compiling output

The default options is `-make` when a module is part of the parameters,
compiles all the way to the linking phase and executable creation.
Here there is no `Main` module so no executable is created.
- Compiling with `ghc --make Foo.hs` generates files `Foo.hi` and `Foo.o`
with output:
```shellsession
$ ghc --make Foo.hs
[1 of 1] Compiling Foo              ( Foo.hs, Foo.o )
```
- Compiling with `ghc -c Foo.hs` generates the same files `Foo.hi` and
`Foo.o` with no output.
The flag `-c` makes the compiler stop just before the last phase (linker).

### Object file info
Using GNU binary utilities (binutils) `objdump` command on Foo.o:
```shellsession
$ objdump --file-headers Foo.o
```
```assembly
Foo.o:     file format elf64-x86-64
architecture: i386:x86-64, flags 0x00000011:
HAS_RELOC, HAS_SYMS
start address 0x0000000000000000
```
```shellsession
$ objdump --syms Foo.o
```
```assembly
Foo.o:     file format elf64-x86-64

SYMBOL TABLE:
0000000000000000 l    d  .text  0000000000000000 .text
0000000000000000 l    d  .data  0000000000000000 .data
0000000000000000 l    d  .bss   0000000000000000 .bss
0000000000000000 l    d  .rodata        0000000000000000 .rodata
0000000000000000 l       .rodata        0000000000000000 c14n_str
0000000000000000 l       .data  0000000000000000 rYT_closure
0000000000000008 l       .rodata        0000000000000000 c14r_str
0000000000000010 l       .data  0000000000000000 r14g_closure
0000000000000040 l       .data  0000000000000000 s14l_closure
0000000000000000 l       .data.rel.ro   0000000000000000 S14H_srt
0000000000000000 l    d  .data.rel.ro   0000000000000000 .data.rel.ro
0000000000000000 g     O .data  0000000000000000 __stginit_Foo
0000000000000000         *UND*  0000000000000000 ghczmprim_GHCziTypes_TrNameS_static_info
0000000000000020 g     O .data  0000000000000000 Foo_zdtrModule_closure
0000000000000000         *UND*  0000000000000000 ghczmprim_GHCziTypes_Module_static_info
0000000000000000         *UND*  0000000000000000 ghczmprim_GHCziTypes_Czh_static_info
0000000000000050 g     O .data  0000000000000000 Foo_foo_closure
0000000000000018 g     O .text  0000000000000057 Foo_foo_info
0000000000000000         *UND*  0000000000000000 _GLOBAL_OFFSET_TABLE_
0000000000000000         *UND*  0000000000000000 newCAF
0000000000000000         *UND*  0000000000000000 stg_bh_upd_frame_info
0000000000000000         *UND*  0000000000000000 base_GHCziShow_zdfShowChar_closure
0000000000000000         *UND*  0000000000000000 base_SystemziIO_print_closure
0000000000000000         *UND*  0000000000000000 stg_ap_pp_fast
```
```shellsession
$ objdump --dynamic-syms Foo.o
```
```assembly
Foo.o:     file format elf64-x86-64

objdump: Foo.o: not a dynamic object
DYNAMIC SYMBOL TABLE:
no symbols
```

### Static Vs. Dynamic

The default is the `-static` flag

> **-static**
> 
>> Tell the linker to avoid shared Haskell libraries, if possible. This is
>> the default.


> **-dynamic**
> 
>> When generating code, assume that entities imported from a different package 
>> will reside in a different shared library or binary.
>>
>> Note that using this option when linking causes GHC to link against shared
>> libraries.

Compiling with `-dynamic` the object file changes a little

```shellsession
$ diff FooStatic.o_syms FooDynamic.o_syms
```
```diff
2c2
< FooStatic.o:     file format elf64-x86-64
---
> FooDynamic.o:     file format elf64-x86-64
22c22,23
< 0000000000000018 g     O .text	000000000000004e Foo_foo_info
---
> 0000000000000018 g     O .text	0000000000000057 Foo_foo_info
> 0000000000000000         *UND*	0000000000000000 _GLOBAL_OFFSET_TABLE_
```

The option `-dynamic-too` allows to do both on the same pass sharing intermediate
steps and creates files Foo.hi, Foo.o, Foo.dyn_hi and Foo.dyn_o


> **-dynamic-too**
>
>> Generates both dynamic and static object files in a single run of GHC. This 
>> option is functionally equivalent to running GHC twice, the second time adding
>> -dynamic -osuf dyn_o -hisuf dyn_hi.
>>
>> Although it is equivalent to running GHC twice, using -dynamic-too is more 
>> efficient, because the earlier phases of the compiler up to code generation are
>> performed just once.
>>
>> When using -dynamic-too, the options -dyno, -dynosuf, and -dynhisuf are the 
>> counterparts of -o, -osuf, and -hisuf respectively, but applying to the dynamic
>> compilation.


But when combined **with `-dynamic-too` the dynamic objects are not equal** as if calling
directly with `-dynamic` only, (both statics yes). It appears that the renamer or an ID
generator shares a counter that helps producing unique names, but you can't trust that
the final code is the same.

```shellsession
$ diff FooDynamic.o_syms FooDynamicToo.dyn_o_syms
```
```diff
2c2
< FooDynamic.o:     file format elf64-x86-64
---
> FooDynamicToo.dyn_o:     file format elf64-x86-64
9c9
< 0000000000000000 l       .rodata	0000000000000000 c14n_str
---
> 0000000000000000 l       .rodata	0000000000000000 c14T_str
11c11
< 0000000000000008 l       .rodata	0000000000000000 c14r_str
---
> 0000000000000008 l       .rodata	0000000000000000 c14X_str
13,14c13,14
< 0000000000000040 l       .data	0000000000000000 s14l_closure
< 0000000000000000 l       .data.rel.ro	0000000000000000 S14H_srt
---
> 0000000000000040 l       .data	0000000000000000 s14R_closure
> 0000000000000000 l       .data.rel.ro	0000000000000000 S15d_srt
```





Most important part!

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/shared_libs.html#using-shared-libs
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/shared_libs.html#finding-shared-libs

6.12.1. Building programs that use shared libraries

To build a simple program and have it use shared libraries for the runtime system and the base libraries use the -dynamic flag:

ghc --make -dynamic Main.hs

This has two effects. The first is to compile the code in such a way that it can be linked against shared library versions of Haskell packages (such as base). The second is when linking, to link against the shared versions of the packages’ libraries rather than the static versions. Obviously this requires that the packages were built with shared libraries. On supported platforms GHC comes with shared libraries for all the core packages, but if you install extra packages (e.g. with Cabal) then they would also have to be built with shared libraries (--enable-shared for Cabal).
