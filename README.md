
# eclds

<!-- badges: start -->
<!-- badges: end -->

This package provides functions to scrape Minnesota Early Childhood Longitudinal Data System [website](http://eclds.mn.gov/).

## Installation

You can install the development version of eclds with:

``` r
remotes::install_github("hktosun/eclds")
```

## Usage

You need a Selenium Docker image running to use this package. Please follow the instructions provided [here](https://docs.ropensci.org/RSelenium/articles/docker.html). 

To scrape the individuals sections in the website, use the function `scrape_eclds`.

``` r
eclds::scrape_eclds(section = 'kindergarten', subsection = 'early care and education', 'county', year = 2018:2020)
```
