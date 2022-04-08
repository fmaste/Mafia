
--------------------------------------------------------------------------------

module Distribution.Mafia.Build (

          packagePrefix
        , buildLibraryFromCabal

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import Control.Exception (IOException, catch)
import Control.Monad (filterM, mapM, mapM_, unless, when)
import Data.List (elem, filter, intercalate, nub, partition, sort, words)
import System.IO (writeFile, withFile, IOMode (WriteMode), hPutStrLn)
-- Package: binary.
import qualified Data.Binary as Binary
-- Package: ghc-prim.
import qualified GHC.PackageDb as PackageDb
-- Package: Mafia.
import qualified Distribution.Mafia.Cabal as Cabal
import qualified Distribution.Mafia.GHC as GHC
import qualified Distribution.Mafia.Paths as Paths
import Distribution.Mafia.Types
-- Package: directory.
import qualified System.Directory as Directory
-- Package: filepath.
import qualified System.FilePath as FilePath
-- Package: process.
import qualified System.Process as Process

--------------------------------------------------------------------------------

-- | When creating a package that already exists on the global package db
-- ghc-pkg complains even if I use the --package-db=DIR param or
-- --global-package-db=DIR with a differen folder. It appears that the global db
-- is hardwired no matter what I do.
-- Also I have no way of hidding independent packages of GHC like filepath
-- because for example ghc-boot needs it, so if someone want to use a user-built
-- filepath package and ghc-boot at the same time ghc complains or peeks the one
-- on the global package db.
-- So I append this string to all the packages and add user packages first,
-- before any other package parameter, to every ghc call. I even create the
-- package.conf.d *.conf files with this prefix.
packagePrefix :: String
packagePrefix = "MAFIA-"

--------------------------------------------------------------------------------

-- | For each fileId, be a module name (as used on imports) or a c-source, we
-- have a list of search paths where we found it, and for each path we may found
-- it with different file suffixes (hs, lhs).
type AvailableFiles = [ ( String, [ (FilePath, [String] ) ] ) ]

data Build = Build {
          buildPackageName :: PackageName
        , buildPackageVersion :: Version
        , buildId :: BuildId
        , buildIdDir :: FilePath
        , buildDependencies :: [Dependency]
        , buildSourcesDir :: FilePath
        , buildModules :: [ModuleName]
        , buildModulesHidden :: [ModuleName]
        , buildModulesAvailable :: AvailableFiles
        , buildModulesAvailableBoot :: AvailableFiles
        , buildModulesAvailableSig :: AvailableFiles
        -- Files to make available its definitions directly, no need to include.
        , buildDefines :: [FilePath]
        , buildDefinesAvailable :: AvailableFiles
        , buildIncludes :: [FilePath]
        , buildIncludesAvailable :: AvailableFiles
        , buildIncludesToInstall :: [FilePath]
        , buildCbits :: [FilePath]
        , buildCbitsAvailable :: AvailableFiles
} deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | SHA1 of a file, if this crypto hash is good for Git is good for me!
computeSha1Checksum :: FilePath -> IO String
computeSha1Checksum file = do
        output <- Process.readProcess "sha1sum"
                [
                          "--binary"
                        , file
                ]
                ""
        return $ head $ words output

--------------------------------------------------------------------------------

-- | Returns the search path, the fileId as a string and the suffix.
-- Multiple versions may be obtained.
findFiles :: FilePath             -- ^ The sources folder.
          -> [FilePath]           -- ^ The search paths inside the sources.
          -> [ModuleName]         -- ^ The file ID to look for.
          -> (String -> FilePath) -- ^ An optional transformation from fileId.
          -> [String]             -- ^ The suffixes to look this fileIds.
          -> IO AvailableFiles
findFiles sourcesDir searchPaths fileIds fIdToPath suffixes = mapM
        (\fId -> do
                found <- mapM
                        (\p -> do
                                matches <- filterM
                                        (\s -> Directory.doesFileExist $
                                                sourcesDir
                                                FilePath.</> p
                                                FilePath.</> (fIdToPath fId)
                                                FilePath.<.> s
                                        )
                                        suffixes
                                return (p,matches)
                        )
                        searchPaths
                return (fId,found)
        )
        fileIds

-- | Copy source files to the build destination.
installFiles :: AvailableFiles -> (String -> FilePath) -> FilePath -> FilePath -> Bool -> IO ()
installFiles fileIds fIdToPath sourcesDir installDir errorOnMissing = mapM_
        (\(fId,ps) -> case ps of
                ((s,(x:[])):[]) -> do
                        let fPath = fIdToPath fId
                        let destFile = installDir
                                FilePath.</> fPath
                                FilePath.<.> x
                        let destFolder = FilePath.dropFileName destFile
                        Directory.createDirectoryIfMissing True destFolder
                        Directory.copyFile
                                (
                                        sourcesDir
                                        FilePath.</> s
                                        FilePath.</> fPath
                                        FilePath.<.> x
                                )
                                destFile
                ms -> case concatMap (\(_,ss) -> ss) ms of
                        [] -> if errorOnMissing
                                then error $
                                    "No file named " ++ fId ++ " found."
                                else return ()
                        _ -> error $
                                   "Multiple or ambiguous files named "
                                ++ fId ++ " found."
        )
        fileIds

--------------------------------------------------------------------------------

buildLibraryFromCabal :: PackageName -> Version -> [(String,Bool)] -> IO (Maybe BuildId)
buildLibraryFromCabal packageName packageVersion userFlags = do
        putStrLn $ "Building " ++ packageName ++ " (" ++ packageVersion ++ ")"
        -- The sources directory that must already be installed.
        ------------------------------------------------------------------------
        sourcesDir <- Paths.getCompilerGhcPackageIdSourcesDir
                packageName
                packageVersion
        sourcesExists <- Directory.doesDirectoryExist sourcesDir
        when (not sourcesExists) $ do
                error $ "Folder \"" ++ sourcesDir ++ "\" does not exists."
        -- Read Cabal file.
        ------------------------------------------------------------------------
        let cabalFile = sourcesDir FilePath.</> packageName ++ ".cabal"
        unconfiguredPkg <- Cabal.readUnconfiguredPackageDescriptionOrError
                cabalFile
        unless (Cabal.isPackageBuildTypeSimple unconfiguredPkg) $ do
                error $ "Can't build a custom setup package."
        let configureFlags =
                let
                        pkgFlags = Cabal.packageDefinedFlags unconfiguredPkg
                        userFlagsDefined = filter
                                (\(fn,_) -> elem fn $
                                        map (\(pkgFn,_,_) -> pkgFn) pkgFlags
                                )
                                userFlags
                        pkgFlagsNotUsed = map (\(fn,_,v) -> (fn,v)) $ filter
                                (\(fn,_,_) -> not $ elem fn $
                                        map (\(dFn,_) -> dFn) userFlagsDefined
                                )
                                pkgFlags
                in sort $ nub $ pkgFlagsNotUsed ++ userFlagsDefined
        let pkg = Cabal.configurePackage unconfiguredPkg configureFlags
        -- Create temporary build ID because we don't know its ID yet.
        ------------------------------------------------------------------------
        let tmpBuildId = "tmp"
        -- Create the directory to start building.
        ------------------------------------------------------------------------
        tmpBuildDir <- Paths.getBuildIdDir packageName packageVersion tmpBuildId
        Directory.createDirectoryIfMissing True tmpBuildDir
        -- Create the build data.
        ------------------------------------------------------------------------
        build <- createBuildFromCabal pkg packageName packageVersion tmpBuildId tmpBuildDir
        -- The RTS way we are building that now is just profiling or not.
        ------------------------------------------------------------------------
        let profiling = False
        -- Actual build process and erase tmp dir on error.
        ------------------------------------------------------------------------
        catch
                (buildPackage build profiling)
                (\e -> do
                        putStrLn "Error building package"
                        putStrLn (show (e :: IOException))
                        --Directory.removeDirectoryRecursive tmpBuildDir
                        error ""
                )
        -- Crete build id.
        ------------------------------------------------------------------------
        sha1sum <- computeSha1Checksum $ Paths.buildIdBuildInfoFile tmpBuildDir
        newBuildDir <- Paths.getBuildIdDir packageName packageVersion sha1sum
        newBuildDirExists <- Directory.doesDirectoryExist newBuildDir
        -- TODO FIX HACK
        when newBuildDirExists (Directory.removeDirectoryRecursive newBuildDir)
{--
        if newBuildDirExists
                then do
                        putStrLn "Build already exists."
                else do
--}
        -- I have to create the package DB after moving the folder, the package
        -- cant be moved later.
        Process.callProcess "mv" [tmpBuildDir, newBuildDir]
        createPackageDb
                (build {
                          buildId = sha1sum
                        , buildIdDir = newBuildDir
                })
                profiling
        writeFile
                (Paths.buildIdCabalInfosFile newBuildDir)
                (show $ [ CabalInfo configureFlags ])
        return $ Just sha1sum

--------------------------------------------------------------------------------

createBuildFromCabal :: Cabal.ConfiguredPackage -> PackageName -> Version -> BuildId -> FilePath -> IO Build
createBuildFromCabal pkg packageName packageVersion createBuildId buildDir = do
        -- The sources directory that must already be installed.
        ------------------------------------------------------------------------
        sourcesDir <- Paths.getCompilerGhcPackageIdSourcesDir
                packageName
                packageVersion
        -- Dependency resolution.
        ------------------------------------------------------------------------
        let cabalDependencies = Cabal.packageLibraryDependencies pkg
        let (wiredInDependencies, nonWiredInDependencies) = partition
                (flip elem GHC.wiredInPackages)
                cabalDependencies
        (_,ghcPackages) <- GHC.getPackageDb
        let notInstalledWiredInDependencies = filter
                (not . (flip elem $ map fst ghcPackages))
                wiredInDependencies
        when (notInstalledWiredInDependencies /= []) $ do
                error $ "FATAL: This wired-in dependencies are not installed: "
                        ++ (show notInstalledWiredInDependencies)
        dependencies <- findDependencies nonWiredInDependencies
        -- Find files.
        ------------------------------------------------------------------------
        let modulesSearchPaths = Cabal.packageLibraryModulesSearchPaths pkg
        let moduleNames = Cabal.packageLibraryModules pkg
        let moduleNamesHidden = Cabal.packageLibraryOtherModules pkg
        let includesSearchPaths = Cabal.packageLibraryIncludesSearchPaths pkg
        let includeFiles = Cabal.packageLibraryIncludes pkg
        let includeFilesToInstall = Cabal.packageLibraryIncludesInstall pkg
        let cFiles = Cabal.packageLibrarySourcesC pkg
        foundModules <- findFiles
                sourcesDir
                modulesSearchPaths
                moduleNames
                moduleToFilePath
                ["hs","lhs"]
        foundBoots <- findFiles
                sourcesDir
                modulesSearchPaths
                moduleNames
                moduleToFilePath
                ["hs-boot","lhs-boot"]
        foundSigs <- findFiles
                sourcesDir
                modulesSearchPaths
                moduleNames
                moduleToFilePath
                ["hs-sig","lhs-sig"]
        foundH <- findFiles
                sourcesDir
                includesSearchPaths
                (map FilePath.dropExtension includeFiles)
                id
                ["h"]
        foundC <- findFiles
                sourcesDir
                ["."]
                (map FilePath.dropExtension cFiles)
                id
                ["c"]
        return $ Build {
                  buildPackageName = packageName
                , buildPackageVersion = packageVersion
                , buildId = createBuildId
                , buildIdDir = buildDir
                , buildDependencies = dependencies
                , buildSourcesDir = sourcesDir
                , buildModules = moduleNames
                , buildModulesHidden = moduleNamesHidden
                , buildModulesAvailable = foundModules
                , buildModulesAvailableBoot = foundBoots
                , buildModulesAvailableSig = foundSigs
                , buildDefines = includeFiles
                , buildDefinesAvailable = foundH
                , buildIncludes = includeFiles
                , buildIncludesAvailable = foundH
                , buildIncludesToInstall = includeFilesToInstall
                , buildCbits = cFiles
                , buildCbitsAvailable = foundC
        }

findDependencies :: [PackageName] -> IO [Dependency]
findDependencies = mapM
        (\packageName -> do
                ((version,_):_) <- Paths.getAllPackageVersions packageName
                ((bId,_):_) <- Paths.getAllPackageVersionBuildIds
                        packageName
                        version
                return (packageName,version,bId)
        )

--------------------------------------------------------------------------------

buildPackage :: Build -> Bool -> IO ()
buildPackage build buildWay = do
        let sourcesDir = buildSourcesDir build
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let modulesDir = Paths.buildWayModulesDir wayDir
        let includesDir = Paths.buildWayIncludesDir wayDir
        let cBitsDir = Paths.buildWayCbitsDir wayDir
        -- Create dirs.
        ------------------------------------------------------------------------
        Directory.createDirectoryIfMissing True modulesDir
        Directory.createDirectoryIfMissing True includesDir
        Directory.createDirectoryIfMissing True cBitsDir
        -- Copy source files to compile dir.
        ------------------------------------------------------------------------
        putStrLn "Copying source modules files ..."
        installFiles
                (buildModulesAvailable build)
                moduleToFilePath
                sourcesDir
                modulesDir
                True
        putStrLn "Copying source modules boot files ..."
        installFiles
                (buildModulesAvailableBoot build)
                moduleToFilePath
                sourcesDir
                modulesDir
                False
        putStrLn "Copying source modules signature files ..."
        installFiles
                (buildModulesAvailableSig build)
                moduleToFilePath
                sourcesDir
                modulesDir
                False
        putStrLn "Copying source define files ..."
        installFiles
                (buildDefinesAvailable build)
                id
                sourcesDir
                includesDir
                True
        putStrLn "Copying source include files ..."
        installFiles
                (buildIncludesAvailable build)
                id
                sourcesDir
                includesDir
                True
        putStrLn "Copying source c files ..."
        installFiles
                (buildCbitsAvailable build)
                id
                sourcesDir
                cBitsDir
                True
        -- Compiler pipeline.
        ------------------------------------------------------------------------
        autogenMacros build buildWay
        compileCbits build buildWay
        preprocessModules build buildWay
        compileModules build buildWay
        createModulesLibraryStatic build buildWay
        createModulesLibraryDynamic build buildWay
        -- Build ID.
        ------------------------------------------------------------------------
        cleanDumpedFilesSuffix build buildWay "parsed"
        createBuildDigest build buildWay
        return ()

--------------------------------------------------------------------------------

{-- TODO

/* tool gcc-5.4.0 */
#define TOOL_VERSION_gcc "5.4.0"
#define MIN_TOOL_VERSION_gcc(major1,major2,minor) (\
  (major1) <  5 || \
  (major1) == 5 && (major2) <  4 || \
  (major1) == 5 && (major2) == 4 && (minor) <= 0)

/* tool ghc-8.0.1 */
#define TOOL_VERSION_ghc "8.0.1"
#define MIN_TOOL_VERSION_ghc(major1,major2,minor) (\
  (major1) <  8 || \
  (major1) == 8 && (major2) <  0 || \
  (major1) == 8 && (major2) == 0 && (minor) <= 1)

/* tool ghc-pkg-8.0.1 */
#define TOOL_VERSION_ghc_pkg "8.0.1"
#define MIN_TOOL_VERSION_ghc_pkg(major1,major2,minor) (\
  (major1) <  8 || \
  (major1) == 8 && (major2) <  0 || \
  (major1) == 8 && (major2) == 0 && (minor) <= 1)

/* tool haddock-2.17.2 */
#define TOOL_VERSION_haddock "2.17.2"
#define MIN_TOOL_VERSION_haddock(major1,major2,minor) (\
  (major1) <  2 || \
  (major1) == 2 && (major2) <  17 || \
  (major1) == 2 && (major2) == 17 && (minor) <= 2)

/* tool happy-1.19.5 */
#define TOOL_VERSION_happy "1.19.5"
#define MIN_TOOL_VERSION_happy(major1,major2,minor) (\
  (major1) <  1 || \
  (major1) == 1 && (major2) <  19 || \
  (major1) == 1 && (major2) == 19 && (minor) <= 5)

/* tool hpc-0.67 */
#define TOOL_VERSION_hpc "0.67"
#define MIN_TOOL_VERSION_hpc(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  67 || \
  (major1) == 0 && (major2) == 67 && (minor) <= 0)

/* tool hsc2hs-0.68 */
#define TOOL_VERSION_hsc2hs "0.68"
#define MIN_TOOL_VERSION_hsc2hs(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  68 || \
  (major1) == 0 && (major2) == 68 && (minor) <= 0)

/* tool pkg-config-0.29.1 */
#define TOOL_VERSION_pkg_config "0.29.1"
#define MIN_TOOL_VERSION_pkg_config(major1,major2,minor) (\
  (major1) <  0 || \
  (major1) == 0 && (major2) <  29 || \
  (major1) == 0 && (major2) == 29 && (minor) <= 1)

/* tool strip-2.26 */
#define TOOL_VERSION_strip "2.26"
#define MIN_TOOL_VERSION_strip(major1,major2,minor) (\
  (major1) <  2 || \
  (major1) == 2 && (major2) <  26 || \
  (major1) == 2 && (major2) == 26 && (minor) <= 0)

#define CURRENT_COMPONENT_ID "bytestring-0.10.8.1-9P8r3KMdExw2ImvfwCFxyC"

#define CURRENT_PACKAGE_KEY "bytestring-0.10.8.1-9P8r3KMdExw2ImvfwCFxyC"

--}

-- | Define all macros and variables for the C preprocessor.
autogenMacros :: Build -> Bool -> IO ()
autogenMacros build buildWay = do
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let autogenDir = Paths.buildWayAutogenDir wayDir
        Directory.createDirectoryIfMissing True autogenDir
        withFile
                (Paths.buildWayAutogenMacrosFile wayDir)
                WriteMode
                (\h -> mapM_
                        (\(n,v,_) -> do
                                -- #define VERSION_array "0.5.1.1"
                                hPutStrLn h $ "#define"
                                        ++ " " ++ "VERSION_" ++ n
                                        ++ " " ++ "\"" ++ v ++ "\""
                                hPutStrLn h $ autogenMacroFunction n v
                                hPutStrLn h ""
                        )
                        (buildDependencies build)
                )

-- | Define the macro function with the package version for the C preprocessor.
--
-- #define MIN_VERSION_PACKAGENAME(major1,major2,major3) (\
--   (major1) <  5 || \
--   (major1) == 5 && (major2) <  4 || \
--   (major1) == 5 && (major2) == 4 && (minor) <= 0)
autogenMacroFunction :: PackageName -> Version -> String
autogenMacroFunction pn v =
        let (v1:v2:v3:[]) = take 3 $ (versionToInts v) ++ (repeat 0)
        in
                   "#define MIN_VERSION_" ++ pn ++ "(major1,major2,major3) ("
                ++ "\\\n"
                ++          " (major1) <  " ++ (show v1)
                ++ " || "
                ++ "\\\n"
                ++          " (major1) == " ++ (show v1)
                ++     " && "
                ++          " (major2) <  " ++ (show v2)
                ++ " || "
                ++ "\\\n"
                ++          " (major1) == " ++ (show v1)
                ++     " && "
                ++          " (major2) == " ++ (show v2)
                ++     " && "
                ++          " (minor) <= " ++ (show v3)
                ++ ")"

packageParams :: Build -> Bool -> IO [String]
packageParams build buildWay = do
        mafiaParams <- mafiaPackagesParams buildWay (buildDependencies build)
        ghcParams <- ghcPackagesParams
        return $
                [
                        -- First clear all!
                          "-clear-package-db"
                        , "-hide-all-packages"
                ]
                ++
                -- First our own packages are made visible.
                mafiaParams
                ++
                -- Them only the requested wired-in packages.
                -- The others are hidden and none independent packages.
                ghcParams

ghcPackagesParams :: IO [String]
ghcPackagesParams = do
        (ghcPackageDb, allGhcPackages) <- GHC.getPackageDb
        let ghcWiredIdPackages = filter
                (\(name,_) -> elem name GHC.wiredInPackages)
                allGhcPackages
        let ghcIndependentPackages = filter
                (\(name,_) -> elem name GHC.independentPackages)
                allGhcPackages
        -- Create a list of base packages and version to inform GHC to use.
        let wiredIdPackagesParams = map
                (\(name,version) -> "-package-id" ++ name ++ "-" ++ version)
                ghcWiredIdPackages
        -- Explicitly ignore all the others.
        let independentPackagesParams = map
                (\(name,version) -> "-hide-package" ++ name ++ "-" ++ version)
                ghcIndependentPackages
        return $
                [
                        ("-package-db" ++ ghcPackageDb)
                ]
                ++
                wiredIdPackagesParams
                ++
                independentPackagesParams

mafiaPackagesParams :: Bool -> [Dependency] -> IO [String]
mafiaPackagesParams buildWay dependencies = do
        multiParams <- mapM
                (\(packageName, packageVersion, bId) -> do
                        buildDir <- Paths.getBuildIdDir
                                packageName
                                packageVersion
                                bId
                        let wayDir = Paths.buildIdWayDir buildDir buildWay
                        let packagedbDir = Paths.buildWayPackageDbDir wayDir
                        return
                                [
                                          "-package-db" ++ packagedbDir
                                        , "-package-id"
                                                ++ packagePrefix
                                                ++ packageName
                                ]
                )
                dependencies
        return $ concat multiParams

compileCbits  :: Build -> Bool -> IO ()
compileCbits build buildWay = do
        putStrLn $ "Compile cbits ..."
        pkgParams <- packageParams build buildWay
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let includesDir = Paths.buildWayIncludesDir wayDir
        let autogenMacrosFile = Paths.buildWayAutogenMacrosFile wayDir
        let definesPaths = map
                (\ (d,((_,(sf:_)):_)) ->
                        includesDir FilePath.</> d FilePath.<.> sf
                )
                (buildDefinesAvailable build)
        let cBitsDir = Paths.buildWayCbitsDir wayDir
        let fileNames = map
                (\ (fileName,((_,(_:[])):[])) -> fileName)
                (buildCbitsAvailable build)
        -- GHC uses -fPIC only when called with -dynamic.
        mapM_
                (\fileName -> GHC.runGhc $
                        [
                                  "-c"
                                , "-cpp"
                                , "-fforce-recomp"
                                , "-optP-include"
                                , ("-optP" ++ autogenMacrosFile)
                                , ("-I" ++ includesDir)
                                , (
                                        cBitsDir FilePath.</>
                                        fileName FilePath.<.> "c"
                                )
                                , "-o"
                                , (
                                        cBitsDir FilePath.</>
                                        fileName FilePath.<.> "o"
                                )
                        ]
                        ++
                        (concat $ map
                                (\fp -> ["-optP-include","-optP" ++ fp])
                                definesPaths
                        )
                        ++
                        pkgParams
                )
                fileNames
        mapM_
                (\fileName -> GHC.runGhc $
                        [
                                  "-c"
                                , "-cpp"
                                , "-fforce-recomp"
                                , "-optP-include"
                                , ("-optP" ++ autogenMacrosFile)
                                , ("-I" ++ includesDir)
                                , (
                                        cBitsDir FilePath.</>
                                        fileName FilePath.<.> "c"
                                )
                                , "-o"
                                , (
                                        cBitsDir FilePath.</>
                                        fileName FilePath.<.> "dyn_o"
                                )
                                , "-fPIC"
                        ]
                        ++
                        (concat $ map
                                (\fp -> ["-optP-include","-optP" ++ fp])
                                definesPaths
                        )
                        ++
                        pkgParams
                )
                fileNames

{-- How GHC and Cabal call cpp:
cppSourceCode :: 
/usr/bin/gcc -E -undef -traditional -DINTEGER_GMP
        -include dist/build/autogen/cabal_macros.h -I include
        -I /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/base-4.9.0.0/include
        -I /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/integer-gmp-1.0.0.1/include
        -I /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/include
        '-D__GLASGOW_HASKELL__=800'
        -include /opt/haskell/ghc/8.0.1/lib/ghc-8.0.1/include/ghcversion.h
        '-Dlinux_BUILD_OS=1'
        '-Dx86_64_BUILD_ARCH=1'
        '-Dlinux_HOST_OS=1'
        '-Dx86_64_HOST_ARCH=1'
        '-D__GLASGOW_HASKELL_TH__=1'
        '-D__SSE__=1'
        '-D__SSE2__=1'
        -include/tmp/ghc30640_0/ghc_4.h
        -x assembler-with-cpp ./Data/ByteString/Char8.hs
        -o /tmp/ghc30640_0/ghc_3.hscpp
--}

preprocessModules :: Build -> Bool -> IO ()
preprocessModules build buildWay = do
        putStrLn $ "Preprocessing modules ..."
        pkgParams <- packageParams build buildWay
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let includesDir = Paths.buildWayIncludesDir wayDir
        let autogenMacrosFile = Paths.buildWayAutogenMacrosFile wayDir
        let definesPaths = map
                (\ (d,((_,(sf:_)):_)) ->
                        includesDir FilePath.</> d FilePath.<.> sf
                )
                (buildDefinesAvailable build)
        let modulesDir = Paths.buildWayModulesDir wayDir
        let modulePaths = map
                (\ (m,((_,(sf:_)):_)) ->
                        let moduleFilePath = moduleToFilePath m
                        in ( modulesDir FilePath.</> moduleFilePath, sf)
                )
                (buildModulesAvailable build)
        mapM_
                (\(file,suffix) -> GHC.runGhc $
                        [
                                  "-E"
                                , "-cpp"
                                , "-fforce-recomp"
                                , "-optP-include"
                                , ("-optP" ++ autogenMacrosFile)
                                , ("-I" ++ includesDir)
                                , (file FilePath.<.> suffix)
                                , "-o"
                                , (file FilePath.<.> (suffix ++ "pp"))
                        ]
                        ++
                        (concat $ map
                                (\fp -> ["-optP-include","-optP" ++ fp])
                                definesPaths
                        )
                        ++
                        pkgParams
                )
                modulePaths

compileModules :: Build -> Bool -> IO ()
compileModules build buildWay = do
        putStrLn $ "Compiling modules ..."
        pkgParams <- packageParams build buildWay
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let includesDir = Paths.buildWayIncludesDir wayDir
        let autogenMacrosFile = Paths.buildWayAutogenMacrosFile wayDir
        let definesPaths = map
                (\ (d,((_,(sf:_)):_)) ->
                        includesDir FilePath.</> d FilePath.<.> sf
                )
                (buildDefinesAvailable build)
        let tmpDir = Paths.buildWayTmpDir wayDir
        Directory.createDirectoryIfMissing True tmpDir
        -- Module names as "A.B.C".
        let moduleNames = map (\(m,_) -> m) (buildModulesAvailable build)
        let modulesDir = Paths.buildWayModulesDir wayDir
        let interfacesDir = Paths.buildWayInterfacesDir wayDir
        Directory.createDirectoryIfMissing True interfacesDir
-- Flags not used:
-- "-keep-hc-files": Only when compiling via c.
-- "-hcsuf", "hc": Same as above.
-- "-keep-llvm-files": Needs LLVM installed, via llvm.
        GHC.runGhc $
                [
                        -- Mode of operation:
                          "--make"
                        , "-c"
                        , "-fforce-recomp"

                        -- Preprocessor.
                        -- GHC is not picking the .hspp file I generated but I
                        -- still leave them to for debugging purposes.
                        , "-optP-include"
                        , ("-optP" ++ autogenMacrosFile)
                        , ("-I" ++ includesDir)

                        -- Code generation:
                        , "-fasm" -- Use the default, native code generator.

                        -- Building and linking:
                        , "-dynamic-too" -- Get dynamic "for free".
                        , ("-this-unit-id" ++ packagePrefix ++ (buildPackageName build)) -- Or "-package-name"
                        --, "-fPIC"

                        -- Intermediate files:
                        , "-fwrite-interface" -- Force it just in case.
                        , "-fobject-code" -- Force it just in case.
                        , "-keep-s-files" -- This is not the default.

                        -- Keep tmp files:
                        , "-keep-tmp-files" -- The temporary folder created.
                        , "-tmpdir", tmpDir -- Usually located on /tmp/ghc*.

                        -- Files suffixes:
                        , "-hisuf", "hi"
                        , "-dynhisuf", "dyn_hi"
                        , "-osuf", "o"
                        , "-dynosuf", "dyn_o"

                        -- File locations:
                        , ("-outputdir" ++ modulesDir)
                        , ("-dumpdir" ++ modulesDir)
                        , ("-hidir" ++ interfacesDir)
                        , ("-odir" ++ modulesDir)
                        , ("-stubdir" ++ modulesDir)

                        -- Module's search paths:
                        , "-i"
                        , ("-i" ++ modulesDir)

                        -- Misc:
                        , "-freverse-errors" -- I'm really cool or just stupid ?

                ]
                ++
                (concat $ map
                        (\fp -> ["-optP-include","-optP" ++ fp])
                        definesPaths
                )
                -- Dumps.
                -- Without "-ddump-to-file" dumps go to stdout/stderr.
                ++ (("-ddump-to-file"):GHC.dumpAllFlags)
                ++ ["-dsuppress-all","-dsuppress-uniques", "-dno-debug-output"]
                ++ pkgParams
                ++ moduleNames
        return ()

createModulesLibraryStatic :: Build -> Bool -> IO ()
createModulesLibraryStatic build buildWay = do
        putStrLn $ "Creating static library archive ..."
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let modulesDir = Paths.buildWayModulesDir wayDir
        let cBitsDir = Paths.buildWayCbitsDir wayDir
        let modulesOFilesPaths = map
                (\(m,_) -> modulesDir FilePath.</> (moduleToFilePath m) ++ ".o")
                (buildModulesAvailable build)
        let cbitsOFilesPaths = map
                (\(f,_) -> cBitsDir FilePath.</> f ++ ".o")
                (buildCbitsAvailable build)
        let libDir = Paths.buildWayLibDir wayDir
        Directory.createDirectoryIfMissing True libDir
        libName <- Paths.libFileNameStatic (buildPackageName build)
        Process.callProcess "ar" $
                [
                          "cqs"
                        , (libDir FilePath.</> libName)
                ]
                ++
                modulesOFilesPaths
                ++
                cbitsOFilesPaths

createModulesLibraryDynamic :: Build -> Bool -> IO ()
createModulesLibraryDynamic build buildWay = do
        putStrLn $ "Creating dynamic library archive ..."
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let modulesDir = Paths.buildWayModulesDir wayDir
        let cBitsDir = Paths.buildWayCbitsDir wayDir
        let modulesOFilesPaths = map
                (\(m,_) -> modulesDir FilePath.</> (moduleToFilePath m) ++ ".dyn_o")
                (buildModulesAvailable build)
        let cbitsOFilesPaths = map
                (\(f,_) -> cBitsDir FilePath.</> f ++ ".dyn_o")
                (buildCbitsAvailable build)
        let libDir = Paths.buildWayLibDir wayDir
        Directory.createDirectoryIfMissing True libDir
        libName <-Paths.libFileNameDynamic (buildPackageName build)
        GHC.runGhc $
                [
                          "-shared"
                        , "-dynamic"
                        , "-o"
                        , (libDir FilePath.</> libName)
                ]
                ++
                modulesOFilesPaths
                ++
                cbitsOFilesPaths

-- | Remove the timestamps from the intermediate .dump-* files.
-- "2016-08-16 21:41:04.888521117 UTC"
cleanDumpedFilesSuffix :: Build -> Bool -> String -> IO ()
cleanDumpedFilesSuffix build buildWay suffix = do
        putStrLn $ "Removing timestamps from dumped " ++ suffix ++ " files ..."
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let modulesDir = Paths.buildWayModulesDir wayDir
        let moduleNames = map (\(m,_) -> m) (buildModulesAvailable build)
        mapM_
                (\m -> do
                        let moduleFilePath = moduleToFilePath m
                        output <- Process.readProcess "sed"
                                [
                                          "/^"
                                                ++ "[0-9]\\{4\\}"
                                                ++ "-"
                                                ++ "[0-9]\\{2\\}"
                                                ++ "-"
                                                ++ "[0-9]\\{2\\}"
                                                ++ "[[:space:]]"
                                                ++ "[0-9]\\{2\\}"
                                                ++ "\\:"
                                                ++ "[0-9]\\{2\\}"
                                                ++ "\\:"
                                                ++ "[0-9]\\{2\\}"
                                                ++ "\\."
                                                ++ "[0-9]\\{0,15\\}"
                                                ++ "[[[:space:]]"
                                                ++ "[A-Z]\\{3\\}"
                                          ++ "$/d"
                                        , (modulesDir ++ "/"
                                                ++ moduleFilePath
                                                ++ ".dump-" ++ suffix
                                        )
                                ]
                                ""
                        writeFile
                                (modulesDir
                                        FilePath.</> moduleFilePath
                                        ++ "." ++ suffix
                                )
                                output
                )
                moduleNames

createBuildDigest :: Build -> Bool -> IO ()
createBuildDigest build buildWay = do
        putStrLn $ "Creating build digest ..."
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let dependencies = buildDependencies build
        let modulesDir = Paths.buildWayModulesDir wayDir
        modulesHashes <- getModulesHashes
                modulesDir
                (buildModulesAvailable build)
                "parsed"
        writeFile
                (Paths.buildIdBuildInfoFile buildDir)
                (show $ BuildInfo
                        []
                        dependencies
                        (buildModules build)
                        (buildModulesHidden build)
                        modulesHashes
                )

getModulesHashes :: FilePath -> AvailableFiles -> String -> IO [(String,String)]
getModulesHashes modulesDir availableModules suffix = do
        let moduleNames = map (\(m,_) -> m) availableModules
        mapM
                (\m -> do
                        let moduleFilePath = moduleToFilePath m
                        sha1sum <- computeSha1Checksum
                                (
                                           modulesDir
                                           FilePath.</> moduleFilePath
                                           ++ "." ++ suffix
                                )
                        return (m,sha1sum)
                )
                moduleNames

createPackageDb :: Build -> Bool -> IO ()
createPackageDb build buildWay = do
        putStrLn $ "Creating package DB ..."
        let buildDir = buildIdDir build
        let wayDir = Paths.buildIdWayDir buildDir buildWay
        let includesDir = Paths.buildWayIncludesDir wayDir
        let packadeDbDir = Paths.buildWayPackageDbDir wayDir
        -- Don't put the files inside the package db folder. If you do this
        -- ghc-pkg complains that the package is already there when registering.
        let packageConfFile =
                wayDir
                FilePath.</>
                (packagePrefix ++ buildPackageName build)
                FilePath.<.>
                 "conf"
        -- Don't create the directory, ghc-pgk creates it.
        GHC.runGhcPkg [ "init", packadeDbDir ]
        let exposed = filter
                (not . (flip elem) (buildModulesHidden build))
                (buildModules build)
        let others = buildModulesHidden build
        writeFile
                packageConfFile
                (
                           "name: " ++
                                packagePrefix ++ (buildPackageName build)
                        ++ "\nversion: " ++
                                (buildPackageVersion build)
                        ++ "\nid: " ++
                                packagePrefix ++ (buildPackageName build)
                        ++ "\nkey: " ++
                                packagePrefix ++ (buildPackageName build)
                        ++ "\nlicense: UnspecifiedLicense"
                        ++ "\nexposed: True"
                        ++ "\nexposed-modules: " ++
                                (intercalate " " exposed)
                        ++ "\nhidden-modules: " ++
                                (intercalate " " others)
                        ++ "\nabi: " ++
                                (buildId build)
                        ++ "\ntrusted: False"
                        ++ "\nimport-dirs: " ++
                                (Paths.buildWayInterfacesDir wayDir)
                        ++ "\nlibrary-dirs: " ++
                                (Paths.buildWayLibDir wayDir)
                        ++ "\ndata-dir: " ++ "TODO"
                        ++ "\nhs-libraries: " ++
                                (Paths.libName $ buildPackageName build)
                        ++ "\ninclude-dirs: " ++
                                includesDir
                        ++ "\nincludes: " ++
                                (intercalate " " $ buildIncludesToInstall build)
                        ++ "\nhaddock-interfaces: "
                        ++ "\nhaddock-html: "
                        ++ "\ndepends: "
                )
        GHC.runGhcPkg
                [
                         "register"
                        , packageConfFile
                        , ("--package-db=" ++ packadeDbDir)
                ]

{-- GHC source code
http://github.com/ghc/ghc

- ghc/Main.hs
Function `doMake` with comment "Run --make mode".
Receives `[(String,Maybe Phase)]`, that's interesting because it means that
every input file can have its own stop phase.
If the are no files with Haskell source on parameters calls `oneShot` from
`main/DriverPipeline.hs` with the other files, maybe object files. This means
don't stop and try to go all the way to the linking phase.
Comment from doMake:
> if we have no haskell sources from which to do a dependency analysis, then
> just do one-shot compilation and/or linking. This means that "ghc Foo.o Bar.o
> -o baz" links the program as we expect.
Else, `doMake` passes every source to `guessTarget` (on `compiler/main/GHC.hs`)
creating a list of `Target` (Defines on `compiler/main/HscTypes.hs`) (Also of
interest if the file starts with '*' the record field `targetAllowObjCode` is
`False`), with this calls `setTargets targets` and `load LoadAllTargets`.
Function `setTargets` from `compiler/main/GHC.hs` and `load` comes from
`compiler/main/GhcMake.hs`. Both function run on the `Ghc` monad, so, creates
a target for each file passed as argument, sets all this target to the actual
`Ghc` monad session that was initiated on `main` of `ghc/Main.hs` (also calling
`getSessionDynFlags`) to call `load` for all this targets.

compileFile

# See `abiHash` on `ghc/Main.hs`:
```
{-
        ghc --abi-hash Data.Foo System.Bar
Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.
This is used by Cabal for generating the ComponentId for a
package.  The ComponentId must change when the visible ABI of
the package chagnes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

-- | Print ABI hash of input modules.
--
-- The resulting hash is the MD5 of the GHC version used (Trac #5328,
-- see 'hiVersion') and of the existing ABI hash from each module (see
-- 'mi_mod_hash').
abiHash :: [String] -- ^ List of module names
-> Ghc ()
```

--}

{--TODO: Look at

https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscPipe

http://www.well-typed.com/blog/30/
https://ghc.haskell.org/trac/ghc/wiki/SharedLibraries/PlatformSupport
http://stackoverflow.com/questions/14270177/ghc-statically-linking-linux-binaries-for-arm-mips-processors
https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/using-shared-libs.html
http://stackoverflow.com/questions/27815467/haskell-dynamic-library
http://www.vex.net/~trebla/haskell/so.xhtml
https://hackage.haskell.org/package/rts-loader


http://stackoverflow.com/questions/5953199/create-a-static-haskell-linux-executable

-this-unit-id⟨unit-id⟩

    Tells GHC the the module being compiled forms part of unit ID ⟨unit-id⟩;
    internally, these keys are used to determine type equality and linker
    symbols. As of GHC 8.0, unit IDs must consist solely of alphanumeric
    characters, dashes, underscores and periods. GHC reserves the right to
    interpret other characters in a special way in later releases.

-library-name⟨hash⟩

    Tells GHC that the source of a Backpack file and its textual dependencies
    is uniquely identified by ⟨hash⟩. Library names are determined by Cabal; a
    usual recipe for a library name is that it is the hash source package
    identifier of a package, as well as the version hashes of all its textual
    dependencies. GHC will then use this library name to generate more unit IDs.

--}
