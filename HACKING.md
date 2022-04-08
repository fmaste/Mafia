Never never ever import a package not bundled with the stock GHC installation
(base, time, bytestring, binary, array, containers, transformers, filepath,
directory, process, unix, pretty, template-haskell and Cabal). Use commands
line utilities like tar, grep, sed, wget, sha1sum if needed. This is to keep
the installation process simple, just calling ghc or ghci on the src directory.

We only support one mayor release of GHC at a time and the version of packages
that come with the minor mayor GHC release that we are supporting.

