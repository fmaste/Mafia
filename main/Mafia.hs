
--------------------------------------------------------------------------------

module Main (main) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import Control.Monad (when)
import Data.List (drop, elem, nub, partition)
import System.Environment (getArgs)
import qualified Distribution.Mafia.Build as Build
import qualified Distribution.Mafia.GHC as GHC
import qualified Distribution.Mafia.Hackage as Hackage
import qualified Distribution.Mafia.Solver as Solver

--------------------------------------------------------------------------------

{-- This ones worked as expected:

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

--}

main :: IO ()
main = do

        Solver.dependenciesOfName "array" >>= (putStrLn . show)
        Solver.dependenciesOfName "deepseq" >>= print
        Solver.dependenciesOfName "bytestring" >>= print

        args <- getArgs
        let (flags,commands) = partition
                (\arg -> arg /= "" && head arg == '-')
                args
        let cleanFlags = filter (/= "") $ map tail flags
        when (length commands == 0) $ do
                putStrLn "No command given"
                error "Commands: [ghc | hackage | cabal]"
        let ((cmd:[]), subCmds) = splitAt 1 commands
        mainCommand cmd subCmds cleanFlags
{--
        cabalFile <- Cabal.findUniqueCabalFileOnDirectoryOrError "."
        print cabalFile
        gpd <- Cabal.readUnconfiguredPackageDescriptionOrError cabalFile
        print cabalFile
        processGenericPackageDescription gpd
--}

mainCommand :: String -> [String] -> [String] -> IO ()
-- GHC
------
mainCommand "ghc" ("version":_) _ = GHC.getVersion >>= (putStrLn . show)
mainCommand "ghc" ("libdir":_) _ = GHC.getLibDir >>= (putStrLn . show)
mainCommand "ghc" ("ways":_) _ = GHC.getRtsLibs >>= (putStrLn . show)
mainCommand "ghc" ("packages":_) _ = do
        (_,packages) <- GHC.getPackageDb
        putStrLn $ show $ filter
                (not . (flip elem GHC.independentPackages) . fst)
                packages
mainCommand "ghc" _ _ = error "Usage: ghc [version | libdir | ways | packages]"
-- HACKAGE
----------
mainCommand "hackage" ("update":_) _ = Hackage.fetchHackageIndex
mainCommand "hackage" ("fetch":packageName:version:_) _ = do
        Hackage.fetchPackageFromHackage packageName version
mainCommand "hackage" ("fetch":_) _ = do
        error "Usage: hackage fetch PACKAGENAME VERSION"
mainCommand "hackage" ("install":packageName:version:_) _ = do
        Hackage.installHackagePackageSources packageName version
mainCommand "hackage" ("install":_) _ = do
        error "Usage: hackage install PACKAGENAME VERSION"
mainCommand "hackage" _ _ = do
        error "Usage: hackage [ update | fetch | install ]"
-- CABAL
--------
mainCommand "cabal" ("library":packageName:version:_) flags = do
        let cabalFlags = map (drop 1) $
                filter (\f -> head f == 'f' && length f > 1) flags
        let positiveFlags = filter (\f -> head f /= '-') cabalFlags
        let negativeFlags = map (drop 1) $
                filter (\f -> head f == '-') cabalFlags
        -- If you have both negative and positive flags you get the default!
        let uniquePositiveFlags = nub $ filter
                (not . (flip elem) negativeFlags)
                positiveFlags
        let uniqueNegativeFlags = nub $ filter
                (not . (flip elem) positiveFlags)
                negativeFlags
        let allFlags =
                   (map (\f -> (f,True )) uniquePositiveFlags)
                ++ (map (\f -> (f,False)) uniqueNegativeFlags)
        Build.buildLibraryFromCabal packageName version allFlags >>= print
mainCommand "cabal" ("library":_) _ = do
        error "Usage: cabal library PACKAGENAME VERSION [-fCABALFLAG/-f-CABALFLAG]"
-- ERROR
--------
mainCommand cmd subCmds _ = error $ cmd ++ " " ++ (concat subCmds)

