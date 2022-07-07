# cortado-fs: high performance 100% F# implementation of XGBoost

## Main features:
* native support for both numeric and categorical data (*covariates* and *factors*)
* innovative feature engineering: virtual data columns and easy conversions between numeric and categorical data
* out of core data processing when dataframes are bigger than RAM
* implementation of XGBoost logistic in just a few hundred lines of F# code, 3x faster than original C++ implementation (tree_method=exact, single threaded)
* easy to extend, written in functional style for easy composition
* easily import data from Feather format
* work in progress - POC, ported from my Python/llvm *cortado* implementation with the same performance characteristics

## How to take it for a spin:
* sample data with 1M observations in Feather format is included in the repo (airlinetrain.feather)
* build from source, no nuget yet available
* run XGRun.fsx script to fit a model (equivalent to oryginal XGBoost implementation with tree_method=exact and single threaded).

## Factors and covariates
Numeric data is represented by *Covariate* type and categorical by *Factor*.
Categorical data is stored as integer indexes pointing to string levels, similar to Feather. 
Is is easy to convert covariate into factor and factor into covariate. See XGRun script.
All operations are lazy and heavily use sequences of memory slices. Slice length is an input parameter for the algorithm and optimal value will depend on CPU cache. Cache friendliness is probably the biggest factor in making the implementation fast.

## More work required
If there is interest in this approach I am happy to transfer the code to any other repo and help in any way I can.

