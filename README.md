# Hacking with the ghc package

## Stephen Diehl's GHC Pipeline

Following Stephen Diehl's GHC Pipeline tutorial on creating a custom mini-ghci. 
There were only a few changes I made to Diehl's version.

- In the `DynFlags` record, `hscTarget` has been renamed to `backend`.
- I chose to qualify most names brought in from `ghc` to make it clear where functions came from.

## Adding `:browse` and `:load`

I added commands to browse the current context and load a file as in ghci. 
Here's an example using `data/Example.hs`:

```
>>> :browse
<prints all names defined in Prelude>
>>> :load data/Example.hs
Successfully loaded file: data/Example.hs
>>> :browse
<all Prelude definitions>
hello
```