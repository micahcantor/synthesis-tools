# Hacking with the ghc package

## Stephen Diehl's GHC Pipeline

Following Stephen Diehl's GHC Pipeline tutorial on creating a custom mini-ghci. 
There were only a few changes I made to Diehl's version.

- In the `DynFlags` record, `hscTarget` has been renamed to `backend`.
- I chose to qualify most names brought in from `ghc` to make it clear where functions came from.