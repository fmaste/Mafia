
--------------------------------------------------------------------------------

module Distribution.Mafia.Solver (

          VersionNumberMatcher (..)
        , VersionMatcher

        , DependencyBuild
        , DependencyName (DependencyName)
        , DependencyVersion
        , dependenciesOfName

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import Control.Monad (foldM)
import Data.List (elem, filter, all, intersperse)
import System.IO (readFile)
-- Package: containers.
import qualified Data.Map.Strict as Map
-- Package: Mafia.
import qualified Distribution.Mafia.GHC as GHC
import qualified Distribution.Mafia.Paths as Paths
import Distribution.Mafia.Types
-- Package: directory.
import qualified System.Directory as Directory

--------------------------------------------------------------------------------

data VersionNumberMatcher = Any | This Int | Lower Bool Int | Upper Bool Int
        deriving (Eq,Ord,Show)

type VersionMatcher = [VersionNumberMatcher]

numberMatches :: VersionNumberMatcher -> Int -> Bool
numberMatches Any _ = True
numberMatches (This x) y = x == y
numberMatches (Lower False x) y = x < y
numberMatches (Lower True x) y = x <= y
numberMatches (Upper False x) y = x > y
numberMatches (Upper True x) y = x >= y

versionMatches :: VersionMatcher -> [Int] -> Bool
versionMatches matcher version = all
        (uncurry numberMatches)
        (zip matcher (version ++ (repeat 0)))

--------------------------------------------------------------------------------

newtype DependencyName = DependencyName (Map.Map PackageName DependencyVersion)
        deriving (Eq)

type DependencyVersion = Map.Map Version DependencyBuild

type DependencyBuild = Map.Map BuildId DependencyName

instance Show DependencyName where
        show (DependencyName depsNames) = concat $ Map.elems $ Map.mapWithKey
                (\name depsVersion -> concat $ Map.elems $ Map.mapWithKey
                        (\version depsBuilds -> concat $ Map.elems $ Map.mapWithKey
                                (\buildId depsNames' ->
                                        name ++ " " ++ version ++ " " ++ buildId
                                        ++ "\n"
                                        ++ (concat $ intersperse "   " $
                                                lines (show depsNames')
                                           )
                                )
                                depsBuilds
                        )
                        depsVersion
                )
                depsNames

-- Circular dependencies should not exist.

-- The PackageName map can be empty, but that only meens that this BuildId has
-- no direct dependencies (Others than the wired-in packages).
-- So if the BuildInfo file is not there return Nothing.
dependencyOfBuild :: PackageName -> Version -> BuildId -> IO (Maybe DependencyName)
dependencyOfBuild name version buildId = do
        buildIdDir <- Paths.getBuildIdDir name version buildId
        let buildInfoFile = Paths.buildIdBuildInfoFile buildIdDir
        -- If the BuildInfo file is not there we assume that something is
        -- wrong with this build folder.
        buildExists <- Directory.doesFileExist buildInfoFile
        if not buildExists
                then return $ Nothing
                else do
                        buildInfoStr <- readFile buildInfoFile
                        let buildInfo = (read buildInfoStr :: BuildInfo)
                        -- There should be no wired-in deps here, but ...
                        let biNonWiredInDeps = filter
                                (\(n,_,_) -> not $ elem n (GHC.wiredInPackages))
                                (buildInfoDependencies buildInfo)
                        ans <- foldM
                                dependencyOfBuild'
                                (DependencyName Map.empty)
                                biNonWiredInDeps
                        return (Just ans)

dependencyOfBuild' :: DependencyName -> (PackageName,Version,BuildId) -> IO DependencyName
dependencyOfBuild' (DependencyName depsNames) (n,v,b) = do
        maybeDepsNames <- dependencyOfBuild n v b
        case maybeDepsNames of
                Nothing -> do
                        error $ "Build not found: " ++ (show (n,v,b))
                (Just ans) -> return $ DependencyName $
                        Map.insert
                                n
                                (Map.singleton v $ Map.singleton b ans)
                                depsNames

dependenciesOfName :: PackageName -> IO DependencyVersion
dependenciesOfName name = do
        -- All known version folders of this package name.
        -- Can be empty, with no builds!
        versions <- Paths.getAllPackageVersions name
        -- For each version create a map of BuildIds.
        foldM
                (\depsVersions (version,_) -> do
                        depsBuilds <- dependenciesOfVersion name version
                        -- The BuildId map can be empty so this is how we know
                        -- if the version is also empty.
                        if Map.null depsBuilds
                                then return depsVersions
                                else return $ Map.insert
                                        version depsBuilds depsVersions
                )
                Map.empty
                versions

dependenciesOfVersion :: PackageName -> Version -> IO DependencyBuild
dependenciesOfVersion name version = do
        -- All known builds folders of this package name and versions.
        buildIds <- Paths.getAllPackageVersionBuildIds name version
        -- For each BuildId look for its dependencies.
        foldM
                (\depsBuilds (buildId, _) -> do
                        -- Check if there is no real build (like an empty
                        -- folder) or the build has no dependencies.
                        maybeDepsNames <- dependencyOfBuild name version buildId
                        case maybeDepsNames of
                                Nothing -> return depsBuilds
                                (Just depsNames) -> return $
                                        Map.insert
                                                buildId
                                                depsNames
                                                depsBuilds
                )
                Map.empty
                buildIds
