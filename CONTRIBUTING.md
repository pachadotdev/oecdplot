# Contributing

Contributions to `oecdplot` are always welcome! This document lays out caveats
to keep in mind for when making contributions to the code or the vignettes.

## Version control

This project is developed in the Algobank. To contribute to the code or 
the documentation, please ensure that you have Git installed and that you 
understand how to branch and create merge requests. For more information on 
using Git at the OECD, please consult the [Unified Git Manual](https://algobank.oecd.org:4430/rpythonalgobank/unified-git-manual)

## Etiquette

For minor fixes or contributions (e.g. correcting typos), please create a merge 
request and tag the maintainer of the package for review.

For larger changes (e.g. adding a new feature), please create an issue first 
and discuss with the package maintainer before submitting a merge request. 

## Style

The code in this package follows the [Tidyverse style guide](https://style.tidyverse.org/).
The source code uses non-standard evaluation, and calls on functions from the 
tidyverse.

## Documenting

When you build the package and have changed scripts in `R/`, please verify that
build tools use `roxygen` to generate the documentation. Otherwise the 
documentation won't reflect your changes and/or the NAMESPACE will remain
unaltered.

## Datasets

`pta` is a dataset obtained from a Statlink. It is used for examples in the 
documentation.

`oecd_colours` was initially made with a combination of Excel and Paint. Since
v0.1.4 you have to source `dev/00-extract-colours.R` in order to read the
*official* XML files from `W:` and create both the tibble with the palettes, and
the image that displays them in the documentation.
