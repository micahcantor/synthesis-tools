# Hacking with the ghc package

## Stephen Diehl's GHC Pipeline

Following Stephen Diehl's GHC Pipeline tutorial on creating a custom mini-ghci. 
There were a few changes with the newer version of GHC I used:

- GHC path functionality has been moved to a separate package, [ghc-paths](https://hackage.haskell.org/package/ghc-paths)
- In the `DynFlags` record, `hscTarget` has been renamed to `backend`.