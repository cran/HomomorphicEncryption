# HomomorphicEncryption
<!-- badges: start -->
  [![License](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
  [![CRAN status](https://www.r-pkg.org/badges/version/HomomorphicEncryption)](https://CRAN.R-project.org/package=HomomorphicEncryption)
  [![R build status](https://github.com/bquast/HomomorphicEncryption/workflows/R-CMD-check/badge.svg)](https://github.com/bquast/HomomorphicEncryption/actions?workflow=R-CMD-check)
  [![Codecov test coverage](https://codecov.io/gh/bquast/HomomorphicEncryption/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bquast/HomomorphicEncryption?branch=main)
  [![Total RStudio Cloud Downloads](https://cranlogs.r-pkg.org/badges/grand-total/HomomorphicEncryption?color=brightgreen)](https://cran.r-project.org/package=HomomorphicEncryption)
[![RStudio Cloud Downloads](https://cranlogs.r-pkg.org/badges/HomomorphicEncryption?color=brightgreen)](https://cran.r-project.org/package=HomomorphicEncryption)
  <!-- badges: end -->

## Installation
`HomomorphicEncryption` can be installed from [CRAN](https://CRAN.R-project.org/package=HomomorphicEncryption) using:
```
install.packages('HomomorphicEncryption')
```

## Development version

You can install the development version of `HomomorphicEncryption` from [GitHub](https://github.com/bquast/HomomorphicEncryption) with:

``` r
if (!require('remotes')) install.packages('remotes')
remotes::install_github('bquast/HomomorphicEncryption', build_vignettes=TRUE)
```

## Usage
Following installation, the package can be loaded using:

```r
library(HomomorphicEncryption)
```

For general information on using the package, please refer to the help files.

```r
help(package='HomomorphicEncryption')
```

The procedures for the various Homomorphic Encrypted schema a described in the vignettes (BFV is the starting point):

```r
vignette(package='HomomorphicEncryption')
```

## Additional Information

An overview of the changes is available in the [NEWS.md](/NEWS.md) file.

```r
news(package='HomomorphicEncryption')
```

## Development

Development takes place on the GitHub page.

https://github.com/bquast/HomomorphicEncryption/

Bugs can be filed on the issues page on GitHub.

https://github.com/bquast/HomomorphicEncryption/issues
