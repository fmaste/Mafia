
--------------------------------------------------------------------------------

module Distribution.Mafia.Paths (

          getDataDir
        , getCacheDir

        , getArchiveHackageDir
        , getArchiveHackageIndexFile
        , getArchiveHackagePackagesDir
        , getArchiveHackagePackageVersionsDir
        , getArchiveHackagePackageIdTarballFile

        , getCompilerGhcDir
        , getCompilerGhcPackagesDir
        , getCompilerGhcPackageVersionsDir
        , getCompilerGhcPackageIdDir
        , getCompilerGhcPackageIdSourcesDir
        , getCompilerGhcPackageIdBuildsDir

        , getBuildIdDir
        , buildIdBuildInfoFile
        , buildIdCabalInfosFile
        , buildIdWayDir

        , buildWayModulesDir
        , buildWayAutogenDir
        , buildWayAutogenMacrosFile
        , buildWayIncludesDir
        , buildWayCbitsDir
        , buildWayInterfacesDir
        , buildWayTmpDir
        , buildWayLibDir
        , buildWayPackageDbDir

        , libName
        , libFileNameStatic
        , libFileNameDynamic

        , getAllPackages
        , getAllPackageVersions
        , getAllPackageVersionBuildIds

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import qualified Distribution.Mafia.GHC as GHC
import Distribution.Mafia.Types
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------

-- | User's Mafia data directory.
-- Mafia is persisted by user (no global, local, etc).
-- If you want to change it use the sudo command or the XDG_DATA_HOME variable.
getDataDir :: IO FilePath
getDataDir = Directory.getXdgDirectory Directory.XdgData "mafia"

-- | A cache also by user.
-- You can delete it every time you want, have fun!
getCacheDir :: IO FilePath
getCacheDir = Directory.getXdgDirectory Directory.XdgCache "mafia"

-- | TODO: Do we need a config? Try not to.
-- It would be great if everything is done with just files and directories, so
-- hacking with scripts and using make files is easier.
getConfigDir :: IO FilePath
getConfigDir = Directory.getXdgDirectory Directory.XdgConfig "mafia"

--------------------------------------------------------------------------------

-- | Hackage files inside the Mafia cache directory.
getArchiveHackageDir :: IO FilePath
getArchiveHackageDir = do
        cacheDir <- getCacheDir
        return $ cacheDir
                FilePath.</> "archive"
                FilePath.</> "hackage.haskell.org"

-- | The index file with all the Hackage packages available.
getArchiveHackageIndexFile :: IO FilePath
getArchiveHackageIndexFile = do
        archiveHackageDir <- getArchiveHackageDir
        return $ archiveHackageDir FilePath.</> "index.tar.gz"

-- | Contains a lists of folders with Hackage package names.
getArchiveHackagePackagesDir :: IO FilePath
getArchiveHackagePackagesDir = do
        archiveHackageDir <- getArchiveHackageDir
        return $ archiveHackageDir FilePath.</> "packages"

-- | Contains a lists of folder with Hackage package versions.
getArchiveHackagePackageVersionsDir :: PackageName -> Version -> IO FilePath
getArchiveHackagePackageVersionsDir packageName packageVersion = do
        archiveHackagePackagesDir <- getArchiveHackagePackagesDir
        return $ archiveHackagePackagesDir
                FilePath.</> packageName
                FilePath.</> packageVersion

-- | The original Hackage tarball of an specific package.
getArchiveHackagePackageIdTarballFile ::
           PackageName
        -> Version
        -> IO FilePath
getArchiveHackagePackageIdTarballFile packageName packageVersion = do
        archiveHackagePackageVersionDir <- getArchiveHackagePackageVersionsDir
                packageName
                packageVersion
        return $ archiveHackagePackageVersionDir FilePath.</> "tarball.tar.gz"

--------------------------------------------------------------------------------

-- | Aside from the user, also the GHC version available defines this location.
-- To change this make another GHC version available on GHC or PATH variables.
getCompilerGhcDir :: IO FilePath
getCompilerGhcDir = do
        dataDir <- getDataDir
        ghcVersion <- GHC.getVersion
        return $ dataDir
                FilePath.</> "compilers"
                FilePath.</> "ghc"
                FilePath.</> ghcVersion

-- | Contains a lists of folders with package names.
getCompilerGhcPackagesDir :: IO FilePath
getCompilerGhcPackagesDir = do
        compilerGhcDir <- getCompilerGhcDir
        return $ compilerGhcDir FilePath.</> "packages"

-- | Contains a lists of folders with package versions.
getCompilerGhcPackageVersionsDir :: PackageName -> IO FilePath
getCompilerGhcPackageVersionsDir packageName = do
        compilerGhcPackagesDir <- getCompilerGhcPackagesDir
        return $ compilerGhcPackagesDir FilePath.</> packageName

-- | The top dir with files of an specific package.
getCompilerGhcPackageIdDir :: PackageName -> Version -> IO FilePath
getCompilerGhcPackageIdDir packageName packageVersion = do
        compilerGhcPackagesDir <- getCompilerGhcPackagesDir
        return $ compilerGhcPackagesDir
                FilePath.</> packageName
                FilePath.</> packageVersion

-- | The extracted hackage tarball of an specific package.
-- Edit at your own risk!
getCompilerGhcPackageIdSourcesDir :: PackageName -> Version -> IO FilePath
getCompilerGhcPackageIdSourcesDir packageName packageVersion = do
        compilerGhcPackageVersionDir <- getCompilerGhcPackageIdDir
                packageName
                packageVersion
        return $ compilerGhcPackageVersionDir FilePath.</> "sources"

-- | Where we put the different built bits of an specific package.
-- The ID of a build is known after compiling everything.
getCompilerGhcPackageIdBuildsDir :: PackageName -> Version -> IO FilePath
getCompilerGhcPackageIdBuildsDir packageName packageVersion = do
        compilerGhcPackageVersionDir <- getCompilerGhcPackageIdDir
                packageName
                packageVersion
        return $ compilerGhcPackageVersionDir FilePath.</> "builds"

--------------------------------------------------------------------------------

-- | Create the build directory based on an ID.
getBuildIdDir :: PackageName -> Version -> BuildId -> IO FilePath
getBuildIdDir packageName packageVersion buildId = do
        compilerGhcPackageVersionBuildsDir <- getCompilerGhcPackageIdBuildsDir
                packageName
                packageVersion
        return $ compilerGhcPackageVersionBuildsDir FilePath.</> buildId

-- | The file that describes the build inside this directory.
buildIdBuildInfoFile :: FilePath -> FilePath
buildIdBuildInfoFile buildIdDir =
        buildIdDir FilePath.</> "BuildInfo"

-- | The file that describes the Cabal configuration used inside this directory.
-- Different Cabal flags can lead to the same build ID, so this file contains
-- a list of different Cabal flags used to configure.
buildIdCabalInfosFile :: FilePath -> FilePath
buildIdCabalInfosFile buildIdDir =
        buildIdDir FilePath.</> "CabalInfos"

-- | Given a build ID directory, get the build way directory.
-- A way now is just vanilla or profiling.
buildIdWayDir ::  FilePath -> Bool -> FilePath
buildIdWayDir buildIdDir profiling =
        buildIdDir FilePath.</> (if profiling then "profiling" else "vanilla")

--------------------------------------------------------------------------------

-- | The *.hs directory of this build way directory.
-- Where we store all the modules files to compile and its corresponding *.o and
-- *.dyn_o.
buildWayModulesDir :: FilePath -> FilePath
buildWayModulesDir wayDir = wayDir FilePath.</> "modules"

-- | The autogen *.h/hs directory of this build way directory.
-- Where we put the files that defines the macros that Haskell users expect when
-- using the CPP pragma and the autogenerated Paths_PACKAGENAME module.
buildWayAutogenDir :: FilePath -> FilePath
buildWayAutogenDir wayDir = wayDir FilePath.</> "autogen"

-- | The autogen file with macros for CPP of this build way directory.
-- The .h file that defines the expected macros.
buildWayAutogenMacrosFile :: FilePath -> FilePath
buildWayAutogenMacrosFile wayDir =
    (buildWayAutogenDir wayDir) FilePath.</> "mafia_macros.h"

-- | The *.h directory of this build way directory.
-- The ones that the library definition says.
buildWayIncludesDir :: FilePath -> FilePath
buildWayIncludesDir wayDir = wayDir FilePath.</> "includes"

-- | The *.c directory of this build way directory.
-- Where we store all the sources in c to compile and its corresponding *.o and
-- *.dyn_o.
buildWayCbitsDir :: FilePath -> FilePath
buildWayCbitsDir wayDir = wayDir FilePath.</> "cbits"

-- | The interfaces directory of this build way directory.
-- Where we store all the *.hi and *.dyn_hi files.
buildWayInterfacesDir :: FilePath -> FilePath
buildWayInterfacesDir wayDir = wayDir FilePath.</> "interfaces"

-- | The tmp directory of this build way directory.
-- Where ghc puts its temporary files.
buildWayTmpDir :: FilePath -> FilePath
buildWayTmpDir wayDir = wayDir FilePath.</> "tmp"

-- | The lib directory of this build way directory.
-- Where the *.a and *.so created are installed.
buildWayLibDir :: FilePath -> FilePath
buildWayLibDir wayDir = wayDir FilePath.</> "libraries"

-- | The package DB directory of this build way directory.
-- Where we create the package.conf.d for this build. Every build creates a
-- package DB with just one package, the one built.
buildWayPackageDbDir :: FilePath -> FilePath
buildWayPackageDbDir wayDir = wayDir FilePath.</> "package.conf.d"

--------------------------------------------------------------------------------

-- | The HSpackagename part.
libName :: PackageName -> String
libName packageName = "HS" ++ packageName

-- | The libHSpackagename part.
libFileName :: PackageName -> String
libFileName packageName = "lib" ++ (libName packageName)

-- | The libHSpackagename.a.
libFileNameStatic :: PackageName -> IO String
libFileNameStatic packageName = do
        return $ (libFileName packageName) ++ ".a"

-- | The libHSpackagename-ghc8.0.1.so.
-- Like libHSbytestring-0.10.8.1-ghc8.0.1.so but without the package version.
-- If you don't use something different besides the suffix (.a or .so) between
-- static and dynamic libraries file names (here is just adding "-ghc8.0.1") GNU
-- linker (ld) prefers the .so instead of the .a when linking but ghc thinks it
-- is using static linking.
-- The result: No "-Wl,-rpath-link -Wl,PATH_TO_LIB" is given to gcc and the
-- library is not found when executing.
-- Also ghc adds "-ghc-8.0.1" when using -dynamic, this is hardwired and you
-- can't avoid it if you use ghc to do the linking phase.
libFileNameDynamic :: PackageName -> IO String
libFileNameDynamic packageName = do
        ghcVersion <- GHC.getVersion
        return $ (libFileName packageName) ++ "-ghc" ++ ghcVersion ++ ".so"

--------------------------------------------------------------------------------

-- | All available package folders.
-- The folder may contain just garbage.
getAllPackages ::IO [(PackageName, FilePath)]
getAllPackages = do
        packagesDir <- getCompilerGhcPackagesDir
        dirExists <- Directory.doesDirectoryExist packagesDir
        if not dirExists
                then return []
                else do
                        packages <- Directory.listDirectory packagesDir
                        return $ map
                                (\p ->
                                    (p, packagesDir FilePath.</> p)
                                )
                                packages

-- | All available package versions folders.
-- The folder may contain just garbage.
getAllPackageVersions ::PackageName -> IO [(Version,FilePath)]
getAllPackageVersions packageName = do
        versionsDir <- getCompilerGhcPackageVersionsDir packageName
        dirExists <- Directory.doesDirectoryExist versionsDir
        if not dirExists
                then return []
                else do
                        versions <- Directory.listDirectory versionsDir
                        return $ map
                                (\v ->
                                        (v, versionsDir FilePath.</> v)
                                )
                                versions

-- | All available package version builds folders.
-- The folder may contain just garbage.
getAllPackageVersionBuildIds ::PackageName -> Version -> IO [(BuildId,FilePath)]
getAllPackageVersionBuildIds packageName version = do
        buildsDir <- getCompilerGhcPackageIdBuildsDir packageName version
        dirExists <- Directory.doesDirectoryExist buildsDir
        if not dirExists
                then return []
                else do
                        builds <- Directory.listDirectory buildsDir
                        return $ map
                                (\b ->
                                    (b, buildsDir FilePath.</> b)
                                )
                                builds
