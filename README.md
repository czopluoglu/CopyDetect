
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CopyDetect

<!-- badges: start -->

<!-- badges: end -->

The goal of CopyDetect is to compute several IRT and non-IRT based
response similarity indices proposed in the literature for
multiple-choice examinations such as the Omega index
[(Wollack, 1997)](doi:10.1177/01466216970214002),Generalized Binomial
Test [(van der Linden &
Sotaridona, 2006)](doi:10.3102/10769986031003283),K index, K1 and K2
indices [(Sotaridona &
Meijer, 2002)](doi:10.1111/j.1745-3984.2002.tb01138.x), M4 index
[(Maynes,2014)](https://books.google.com/books?id=Mc_pAwAAQBAJ&pg=PA53&lpg=PA53&dq=Detection+of+non-independent+test+taking+by+similarity+analysis&source=bl&ots=ZPa_OGR2T9&sig=ACfU3U2WTUfqkPnkZoKobtY14pgY98rJVQ&hl=en&sa=X&ved=2ahUKEwjDkIKg-rPpAhXGTd8KHXTuAVkQ6AEwA3oECGAQAQ#v=onepage&q=Detection%20of%20non-independent%20test%20taking%20by%20similarity%20analysis&f=false),
and S1 and S2 indices [(Sotaridona &
Meijer, 2003)](doi:10.1111/j.1745-3984.2003.tb01096.x).

## Installation

You can install the released version of CopyDetect from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("CopyDetect")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("czopluoglu/CopyDetect")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CopyDetect)
#> Loading required package: mirt
#> Loading required package: stats4
#> Loading required package: lattice
## basic example code
```
