# Parsing Randomness: Free Generators Development

This repository contains the Haskell development for _Parsing Randomness_, along with the code that
we used to run our experiments.

If you want to run the experiments, set your parameters in `src/Experiments.hs` and then run
```
stack run
```

Otherwise, if you just want to work with the free generators abstraction, see `src/FreeGen.hs`.

Requires GHC 9.0.2 (we recommend `ghcup` to get started).