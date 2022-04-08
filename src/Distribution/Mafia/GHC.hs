
--------------------------------------------------------------------------------

-- Interesting about GHC:
-- http://www.aosabook.org/en/ghc.html

module Distribution.Mafia.GHC (

          runGcc
        , runGhc
        , readGhc
        , runGhcPkg
        , readGhcPkg

        , getVersion
        , getLibDir

        , Way
        , getRtsLibs

        , wiredInPackages
        , independentPackages
        , getPackageDb

        , dumpAllFlags

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import System.Environment (lookupEnv)
import Data.List (isPrefixOf, isInfixOf, sort)
import Distribution.Mafia.Types
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process as Process

--------------------------------------------------------------------------------

-- | From GCC environment variable or just expect gcc to be on PATH.
getGccExecutable :: IO String
getGccExecutable = do
        maybeGcc <- lookupEnv "GCC"
        case maybeGcc of
                Nothing -> return "gcc"
                (Just gcc) -> return gcc

-- | From GHC environment variable or just expect ghc to be on PATH.
getGhcExecutable :: IO String
getGhcExecutable = do
        maybeGhc <- lookupEnv "GHC"
        case maybeGhc of
                Nothing -> return "ghc"
                (Just ghc) -> return ghc

-- | From GHC_PKG environment variable or just expect ghc-pkg to be on PATH.
getGhcPkgExecutable :: IO String
getGhcPkgExecutable = do
        maybeGhcPkg <- lookupEnv "GHC_PKG"
        case maybeGhcPkg of
                Nothing -> return "ghc-pkg"
                (Just ghcPkg) -> return ghcPkg

--------------------------------------------------------------------------------

runGcc :: [String] -> IO ()
runGcc params = do
        gcc <- getGccExecutable
        Process.callProcess gcc params

runGhc :: [String] -> IO ()
runGhc params = do
        ghc <- getGhcExecutable
        Process.callProcess ghc params

readGhc :: [String] -> IO String
readGhc params = do
        ghc <- getGhcExecutable
        Process.readProcess ghc params ""

runGhcPkg :: [String] -> IO ()
runGhcPkg params = do
        ghcPkg <- getGhcPkgExecutable
        Process.callProcess ghcPkg params

readGhcPkg :: [String] -> IO String
readGhcPkg params = do
        ghcPkg <- getGhcPkgExecutable
        Process.readProcess ghcPkg params ""

--------------------------------------------------------------------------------

-- | Version string of ghc --numeric-version.
getVersion :: IO Version
getVersion = do
        ghcVersionOutput <- readGhc ["--numeric-version"]
        case (lines ghcVersionOutput) of
                (ghcVersion:[]) -> return ghcVersion
                _ -> error $ "Can't parse, "
                        ++ "output of \"ghc --numeric-version\" changed!"

-- | Like "/usr/share/haskell/ghc-8.0.1/lib/ghc-8.0.1".
getLibDir :: IO Version
getLibDir = do
        ghcLibDirOutput <- readGhc ["--print-libdir"]
        case (lines ghcLibDirOutput) of
                (ghcLibDir:[]) -> return ghcLibDir
                _ -> error $ "Can't parse, "
                        ++ "output of \"ghc --print-libdir\" changed!"

--------------------------------------------------------------------------------

{--

fmaste@fmaste:~$ ls /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts*.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_debug.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_l.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_p.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_debug.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_l.a
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_p.a

fmaste@fmaste:~$ ls /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts*.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_debug-ghc8.0.1.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts-ghc8.0.1.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_l-ghc8.0.1.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_debug-ghc8.0.1.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr-ghc8.0.1.so
/opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/rts/libHSrts_thr_l-ghc8.0.1.so

--}

data Way = Static | Dynamic | Threaded | Debug | Profiling | Eventlog
        deriving (Eq, Ord, Show)

getRtsLibs :: IO [ (String, [Way]) ]
getRtsLibs = do
        libDir <- getLibDir
        let rtsDir = libDir FilePath.</> "rts"
        rtsFiles <- Directory.listDirectory rtsDir
        let rtsLibs = filter (isPrefixOf "libHSrts") rtsFiles
        let rtsLibsStatic = filter
                (\fp -> FilePath.takeExtension fp == ".a")
                rtsLibs
        let rtsLibsDynamic = filter
                (\fp -> FilePath.takeExtension fp == ".so")
                rtsLibs
        let others fp =
                        (if isInfixOf "_thr"   fp then [Threaded]  else [])
                ++
                        (if isInfixOf "_debug" fp then [Debug]     else [])
                ++
                        (if isInfixOf "_p"     fp then [Profiling] else [])
                ++
                        (if isInfixOf "_l"     fp then [Eventlog]  else [])
        return $ map (\(fp,ws) -> (fp, sort ws)) $
                        map (\fp -> (fp,  (Static):(others fp) )) rtsLibsStatic
                ++
                        map (\fp -> (fp, (Dynamic):(others fp) )) rtsLibsDynamic

--------------------------------------------------------------------------------

-- Some info on GHC libraries and packages:
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries
-- https://ghc.haskell.org/trac/ghc/browser/packages
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Packages

-- | Wired in packages are totally specific to GHC.
-- This packages come with GHC and using a different version is not supported.
-- Install a different version of GHC if you need other versions, bye bye!.
wiredInPackages :: [PackageName]
wiredInPackages =
{-- Compile a file with ghc -v to look at the ouput (GHC 8.0.1 example):
    wired-in package ghc-prim mapped to ghc-prim-0.5.0.0
    wired-in package integer-gmp mapped to integer-gmp-1.0.0.1
    wired-in package base mapped to base-4.9.0.0
    wired-in package rts mapped to rts
    wired-in package template-haskell mapped to template-haskell-2.11.0.0
    wired-in package ghc mapped to ghc-8.0.1
    wired-in package dph-seq not found.
    wired-in package dph-par not found.
--}
        [
-- Package rts is automatically used, if you try to use it the erros is:
--  <command line>: cannot satisfy -package-id rts-1.0
--      (use -v for more information)
                  "base"
                , "dph-seq"
                , "dph-par"
                , "ghc"
                , "ghc-prim"
                , "integer-gmp"    -- GHC can be compiles with one of the
                , "integer-simple" -- this two integer libraries.
                , "template-haskell"
        ]
        ++
-- This three are not shown as wired-in but its versions always follow GHC
-- version and is the only one shown on hackage.
        [
                  "ghc-boot"
                , "ghc-boot-th"
                , "ghci"
        ]

-- | Independent packages are loosely coupled to GHC, and often maintained by
-- third parties. This packages are part of GHC boot packages but are
-- independent. You can install your own version if you want.
independentPackages :: [PackageName]
independentPackages =
        [
                  "Cabal"
                , "array"
                , "binary"
                , "bytestring"
                , "containers"
                , "deepseq"
                , "directory"
                , "filepath"
                , "haskeline"
                , "hoopl"
                , "hpc"
                , "pretty"
                , "process"
                , "template-haskell"
                , "terminfo"
                , "time"
                , "transformers"
                , "unix"
                , "xhtml", "xhtml-3000"
        ]

-- | Returns the package DB location and the list of the available global 
-- packages with its installed version string.
getPackageDb :: IO (FilePath, [(PackageName,Version)])
getPackageDb = do
        output <- Process.readProcess "ghc-pkg"
                [
                          "--global"
                        , "--no-user-package-db"
                        , "list"
                ]
                ""
        -- Sample output:
        -- /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/package.conf.d
        --      Cabal-1.24.0.0
        --      array-0.5.1.1
        --      base-4.9.0.0
        --      binary-0.8.3.0
        --      bytestring-0.10.8.1
        let (packageDb:packageIds) = map
                (\x -> case words x of
                        [] -> error $ "Can't parse, "
                                ++ "output of \"ghc-pkg list\" changed!"
                        (s:_) -> s
                )
                -- Last element is empty, just the newline.
                (init $ lines output)
        let ghcPackages = map splitPackageId packageIds
        return (packageDb, ghcPackages)

--------------------------------------------------------------------------------

-- Dumps that go to files.
dumpAllFlags :: [String]
dumpAllFlags =
        [

                -- From: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html#dumping-out-compiler-intermediate-structures
                  "-ddump-parsed"
                , "-ddump-rn"
                -- , "-ddump-tc" -- No, always goes to stdout/stderr.
                , "-ddump-splices"
                -- , "-dth-dec-file=⟨file⟩" TODO!
                -- , "-ddump-types" -- No, always goes to stdout/stderr.
                , "-ddump-deriv"
                , "-ddump-ds"
                , "-ddump-spec"
                , "-ddump-rules"
                , "-ddump-rule-firings"
                , "-ddump-rule-rewrites"
                , "-ddump-vect"
                , "-ddump-simpl"
                -- , "-ddump-inlinings" -- No, always goes to stdout/stderr.
                , "-ddump-stranal"
                , "-ddump-str-signatures"
                , "-ddump-cse"
                , "-ddump-worker-wrapper"
                , "-ddump-occur-anal"
                , "-ddump-prep"
                , "-ddump-stg"
                , "-ddump-cmm"
                , "-ddump-opt-cmm"
                , "-ddump-asm"
                -- , "-ddump-llvm" -- No becuase it implies "-fllvm".
                , "-ddump-bcos"
                , "-ddump-foreign"
                , "-ddump-simpl-iterations"
                , "-ddump-simpl-stats"
                -- , "-ddump-if-trace" -- No, always goes to stdout/stderr.
                , "-ddump-tc-trace"
                , "-ddump-vt-trace"
                , "-ddump-rn-trace"
                -- , "-ddump-rn-stats" -- No, always goes to stdout/stderr.
                --, "-ddump-core-stats" -- No, always goes to stdout/stderr.

                -- From: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#other-options-related-to-interface-files
                , "-ddump-hi"
                , "-ddump-hi-diffs"
                , "-ddump-minimal-imports" -- TODO: Open a bug! Are all created on top directory, not heriarchical.

                -- From: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/separate_compilation.html#dependency-generation
                , "-ddump-mod-cycles"

        ]

-- TODO:
-- | https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
ghcFlagsThatAffectCodeGeneration :: [String]
ghcFlagsThatAffectCodeGeneration =
        [
                  "-fconstraint-solver-iterations"
                , "-freduction-depth"
                , "-fcontext-stack"
                , "-firrefutable-tuples" -- "-fno-irrefutable-tuples"
                , "-fdefer-type-errors"
                , "-fdefer-typed-holes"
                , "-O", "-O1", "-O2", "-Odph"
                , "-fmax-simplifier-iterations", "-fsimplifier-phases"
                , "-fdicts-cheap"
                , "-fdicts-strict"
                , "-fdmd-tx-dict-sel"
                , "-fdo-lambda-eta-expansion"
                , "-feager-blackholing"
                , "-fexcess-precision"
                , "-fexpose-all-unfoldings"
        ]
