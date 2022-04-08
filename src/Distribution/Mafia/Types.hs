
--------------------------------------------------------------------------------

module Distribution.Mafia.Types (

          Version
        , PackageName
        , BuildId
        , Dependency
        , ModuleName

        , versionToInts
        , moduleToFilePath
        , splitPackageId
        , unionPackageId

        , BuildInfo (..)

        , CabalInfo (..)

) where

-- Don't use any package that does not come with the official GHC installation.
--------------------------------------------------------------------------------

-- Package: base.
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------

type Version = String

type PackageName = String

type BuildId = String

type Dependency = (PackageName, Version, BuildId)

type ModuleName = String

--------------------------------------------------------------------------------

versionToInts :: Version -> [Int]
versionToInts "" = []
versionToInts v = case span (/= '.') v of
        (x,[]) -> [read x]
        (x,p:ys) -> (read x):(versionToInts ys)

--------------------------------------------------------------------------------

-- | "A.B.C" to "A/B/C".
moduleToFilePath :: ModuleName -> FilePath
moduleToFilePath = map (\c -> if c == '.' then FilePath.pathSeparator else c)

-- | "bytestring-super-0.8.12" to ("bytestring-super", "0.8.12").
splitPackageId :: String -> (PackageName, Version)
splitPackageId packageId = 
        let version = reverse $ takeWhile (/= '-') $ reverse packageId
            name = reverse $
                drop ((length version)+1) $ reverse packageId
        in (name,version)

-- | "bytestring-super" and "0.8.12" to "bytestring-super-0.8.12".
unionPackageId :: PackageName -> Version -> String
unionPackageId packageName packageVersion = packageName ++ "-" ++ packageVersion

--------------------------------------------------------------------------------

data BuildInfo = BuildInfo {
          buildInfoExtraCompilerFlags :: [String]
        , buildInfoDependencies :: [Dependency]
        , buildInfoModules :: [ModuleName]
        , buildInfoModulesHidden :: [ModuleName]
        , buildInfoModulesHashes :: [ (ModuleName, BuildId) ]
} deriving (Eq, Show, Read)

data CabalInfo = CabalInfo {
          cabalInfoFlags :: [ (String, Bool) ]
} deriving (Eq, Show, Read)
