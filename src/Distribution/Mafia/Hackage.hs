
--------------------------------------------------------------------------------

module Distribution.Mafia.Hackage (

          fetchHackageIndex
        , fetchPackageFromHackage
        , installHackagePackageSources

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import Control.Monad (when)
-- Package: Mafia.
import qualified Distribution.Mafia.Paths as Paths
import Distribution.Mafia.Types
-- Package: directory.
import qualified System.Directory as Directory
-- Package: process.
import qualified System.Process as Process

--------------------------------------------------------------------------------

httpGget :: String -> FilePath -> IO ()
httpGget uri destination = Process.callProcess "wget"
        [
                "--output-document=" ++ destination
                , uri
        ]

extractArchiveFolder :: FilePath -> FilePath -> IO ()
extractArchiveFolder tarballPath destination = Process.callProcess "tar"
        [
                  "--extract"
                , "--file"
                , tarballPath
                , "--directory"
                , destination
                -- We assume the first component is a folder, we want to avoid
                -- it and just extract its files on "destination".
                , "--strip-components=1"
        ]

fetchHackageIndex :: IO ()
fetchHackageIndex = do
        putStrLn $ "Fetching Hackage index file ... "
        archiveHackageDir <- Paths.getArchiveHackageDir
        Directory.createDirectoryIfMissing True archiveHackageDir
        indexPath <- Paths.getArchiveHackageIndexFile
        tarballExists <- Directory.doesFileExist indexPath
        if tarballExists
                then do
                        putStrLn $ "A Hackage index already exists on: "
                        putStrLn indexPath
                        putStrLn $ "Remove it to get a new one."
                        return ()
                else httpGget
                        "https://hackage.haskell.org/packages/index.tar.gz"
                        indexPath
        putStrLn $ "Done fetching Hackage index file."

fetchPackageFromHackage :: PackageName -> Version -> IO ()
fetchPackageFromHackage packageName packageVersion = do
        putStrLn $ "Fetching source archive of "
                ++ packageName ++ " (" ++ packageVersion ++ ") ..."
        tarballPath <- Paths.getArchiveHackagePackageIdTarballFile
                packageName
                packageVersion
        tarballExists <- Directory.doesFileExist tarballPath
        if tarballExists
                then do
                        putStrLn "Archive already exists on:"
                        putStrLn tarballPath
                        putStrLn $ "Remove it to get a new one."
                else do
                        tarbalFolder <- Paths.getArchiveHackagePackageVersionsDir
                                packageName
                                packageVersion
                        Directory.createDirectoryIfMissing True tarbalFolder
                        httpGget
                                ("https://hackage.haskell.org/package/"
                                        ++ packageName
                                        ++ "/"
                                        ++ (unionPackageId
                                                packageName
                                                packageVersion
                                           )
                                        ++ ".tar.gz"
                                )
                                tarballPath

installHackagePackageSources :: PackageName -> Version -> IO ()
installHackagePackageSources packageName packageVersion = do
        putStrLn $ "Installing sources of "
                ++ packageName ++ " (" ++ packageVersion ++ ") ..."
        tarballPath <- Paths.getArchiveHackagePackageIdTarballFile
                                packageName
                                packageVersion
        tarballExists <- Directory.doesFileExist tarballPath
        when (not tarballExists) $ do
                error $ "File \"" ++ tarballPath ++ "\" does not exists."
        sourcesDir <- Paths.getCompilerGhcPackageIdSourcesDir
                packageName
                packageVersion
        sourcesExists <- Directory.doesDirectoryExist sourcesDir
        if sourcesExists
                then do
                        putStrLn "Sources already exists on:"
                        putStrLn $ sourcesDir ++ "/"
                        putStrLn $ "Remove folder to get new ones."
                else do
                        Directory.createDirectoryIfMissing True sourcesDir
                        extractArchiveFolder tarballPath sourcesDir
        putStrLn $ "Done installing sources."
