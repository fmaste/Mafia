{-- Note on .h includes and search paths:

Field `includes` is useless:
There is no explicit mention of the files there on any log when using Cabal.

Examples:

- Package bytestring 0.10.8.1 has:

```
library
        c-sources:              cbits/fpstring.c
                                cbits/itoa.c
        include-dirs:           include
        includes:               fpstring.h
        install-includes:       fpstring.h
```

File cbits/fpstring.c does: `#include "fpstring.h"`
Module Data.ByteString.Internal does: `foreign import ccall unsafe "static fpstring.h fps_reverse" c_reverse`

- Package text 1.2.2.1 has:

```
extra-source-files:
        include/*.h

library
        c-sources:    cbits/cbits.c
        include-dirs: include
        includes:
        install-includes:
```

File cbits/cbits.c does: `#include "text_cbits.h"`
Module Data.Text.Encoding does: `#include "text_cbits.h"`

- Package containers has:

```
extra-source-files:
        include/containers.h

Library
        c-sources:
        include-dirs: include
        includes:
        install-includes:
```

Modules Data.Graph does: `#include "containers.h"`

--}

--------------------------------------------------------------------------------

-- Package: base.
module Distribution.Mafia.Cabal (

          findUniqueCabalFileOnDirectoryOrError

        , UnconfiguredPackage
        , readUnconfiguredPackageDescriptionOrError
        , isPackageBuildTypeSimple
        , packageDefinedFlags

        , ConfiguredPackage
        , configurePackage

        , packageLibraryDependencies

        , packageLibraryModulesSearchPaths
        , packageLibraryModules
        , packageLibraryExposesModules
        , packageLibraryOtherModules

        , packageLibraryIncludesSearchPaths
        , packageLibraryIncludes
        , packageLibraryIncludesInstall
        , packageLibraryCppOptions

        , packageLibrarySourcesC

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

import Control.Exception (IOException, catch)
import Control.Monad (mapM_)
import Data.List (intercalate, sort, nub)
import qualified Distribution.Compiler as Compiler
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PdParse
import qualified Distribution.PackageDescription.Configuration as PdConf
import qualified Distribution.System as System
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO (openFile, IOMode (ReadMode), hSetEncoding, utf8, hGetContents)

--------------------------------------------------------------------------------

findUniqueCabalFileOnDirectoryOrError :: FilePath -> IO FilePath
findUniqueCabalFileOnDirectoryOrError directory = do
        files <- Directory.listDirectory directory
        case filter (\fp -> (FilePath.takeExtension fp) == ".cabal") files of
                ([]) -> do
                        putStrLn "No .cabal file found."
                        error ""
                (cabalFile:[]) -> return cabalFile
                ans@(_:_) -> do
                        putStrLn $ "Multiple .cabal files found: " ++ (show ans)
                        error ""

type UnconfiguredPackage = PD.GenericPackageDescription

readUnconfiguredPackageDescriptionOrError :: FilePath -> IO UnconfiguredPackage
readUnconfiguredPackageDescriptionOrError cabalFile = do
        putStrLn $ "Reading cabal file on " ++ (show cabalFile) ++ " ..."
        -- Read the file only using base functions.
        handle <- catch
                (openFile cabalFile ReadMode)
                (\e -> do
                        putStrLn "Error opening file:"
                        putStrLn (show (e :: IOException))
                        error ""
                )
        cabalStr <- catch
                (do
                        hSetEncoding handle utf8
                        hGetContents handle
                )
                (\e -> do
                        putStrLn "Error reading UTF8 file content:"
                        putStrLn (show (e :: IOException))
                        error ""
                )
        case PdParse.parsePackageDescription cabalStr of
                (PdParse.ParseFailed err) -> do
                        putStrLn $ "Parse failed:"
                        putStrLn (show err)
                        error ""
                (PdParse.ParseOk warns gpd) -> do
                        case warns of
                                [] -> return ()
                                ws -> do
                                        putStrLn $ "Parse finished with "
                                                ++ "warnings:"
                                        mapM_ print ws
                        let (Right (pd,_)) = PdConf.finalizePackageDescription
                                []
                                (const True)
                                System.buildPlatform
                                (Compiler.unknownCompilerInfo
                                        Compiler.buildCompilerId
                                        Compiler.NoAbiTag
                                )
                                []
                                gpd
                        return $ gpd {PD.packageDescription = pd}

isPackageBuildTypeSimple :: UnconfiguredPackage -> Bool
isPackageBuildTypeSimple gpd =
        let pd = PD.packageDescription gpd
        in case (PD.buildType pd) of
                Nothing -> True
                (Just PD.Simple) -> True
                _ -> False

-- | Its name, description and default value.
packageDefinedFlags :: UnconfiguredPackage -> [(String, String, Bool)]
packageDefinedFlags gpd = map
        (\flag@(PD.MkFlag {PD.flagName = (PD.FlagName flagName) }) ->
                (flagName, PD.flagDescription flag, PD.flagDefault flag)
        )
        (PD.genPackageFlags gpd)

type ConfiguredPackage = PD.PackageDescription

configurePackage :: UnconfiguredPackage -> [(String,Bool)] -> ConfiguredPackage
configurePackage gpd flags =
        let cabalFlags = map (\(fn,value) -> (PD.FlagName fn, value)) flags
            confAns = PdConf.finalizePackageDescription
                cabalFlags
                (const True)
                System.buildPlatform
                (Compiler.unknownCompilerInfo
                        Compiler.buildCompilerId
                        Compiler.NoAbiTag
                )
                []
                gpd
        in case confAns of
                (Right (pd,_)) -> pd
                (Left deps) -> error $ "Configure error, missing dependencies: "
                        ++ (show deps)

packageLibraryDependencies :: ConfiguredPackage -> [ String ]
packageLibraryDependencies pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) ->
                    let depends = PD.targetBuildDepends $ PD.libBuildInfo lib
                    in map
                                (\(P.Dependency pkgName _) ->
                                        P.unPackageName pkgName
                                )
                                depends

packageLibraryModulesSearchPaths :: ConfiguredPackage -> [FilePath]
packageLibraryModulesSearchPaths pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.hsSourceDirs $ PD.libBuildInfo lib

-- | Return all modules names as string, as used on import.
-- the list is sorted and repeated modules are removed.
packageLibraryModules :: ConfiguredPackage -> [String]
packageLibraryModules pd = sort $ nub $
           (packageLibraryExposesModules pd)
        ++ (packageLibraryOtherModules pd)

-- | Return all modules names as string, as used on import.
-- The list is sorted and repeated modules are removed.
packageLibraryExposesModules :: ConfiguredPackage -> [String]
packageLibraryExposesModules pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> sort $ nub $ map
                        ((intercalate ".") . ModuleName.components)
                        (PD.exposedModules lib)

-- | Return all modules names as string, as used on import.
-- The list is sorted and repeated modules are removed.
packageLibraryOtherModules :: ConfiguredPackage -> [String]
packageLibraryOtherModules pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> sort $ nub $ map
                        ((intercalate ".") . ModuleName.components)
                        (PD.otherModules $ PD.libBuildInfo lib)

-- | From Cabal users guide: A list of directories to search for header files,
-- when preprocessing with c2hs, hsc2hs, cpphs or the C preprocessor, and also
-- when compiling via C.
-- This are part of the search directory needed when doing #include "file.h"
-- inside haskell modules.
packageLibraryIncludesSearchPaths :: ConfiguredPackage -> [FilePath]
packageLibraryIncludesSearchPaths pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.includeDirs $ PD.libBuildInfo lib

-- | From Cabal users guide: A list of header files to be included in any
-- compilations via C. This field applies to both header files that are already
-- installed on the system and to those coming with the package to be installed.
-- These files typically contain function prototypes for foreign imports used by
-- the package.
packageLibraryIncludes :: ConfiguredPackage -> [FilePath]
packageLibraryIncludes pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.includes $ PD.libBuildInfo lib

-- | From Cabal users guide: A list of header files from this package to be
-- installed into $libdir/includes when the package is installed. Files listed
-- in install-includes: should be found in relative to the top of the source
-- tree or relative to one of the directories listed in include-dirs.
-- install-includes is typically used to name header files that contain
-- prototypes for foreign imports used in Haskell code in this package, for
-- which the C implementations are also provided with the package. Note that to
-- include them when compiling the package itself, they need to be listed in the
-- includes: field as well.
packageLibraryIncludesInstall :: ConfiguredPackage -> [FilePath]
packageLibraryIncludesInstall pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.installIncludes $ PD.libBuildInfo lib

-- | From Cabal users guide: Command-line arguments for pre-processing Haskell
-- code. Applies to haskell source and other pre-processed Haskell source like
-- .hsc .chs. Does not apply to C code, that’s what cc-options is for.
packageLibraryCppOptions :: ConfiguredPackage -> [String]
packageLibraryCppOptions pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.cppOptions $ PD.libBuildInfo lib

-- | From Cabal users guide: A list of C source files to be compiled and linked
-- with the Haskell files.
packageLibrarySourcesC :: ConfiguredPackage -> [FilePath]
packageLibrarySourcesC pd =
        case PD.library pd of
                Nothing -> []
                (Just lib) -> PD.cSources $ PD.libBuildInfo lib
