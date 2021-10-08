
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eclds

<!-- badges: start -->
<!-- badges: end -->

This package provides functions to scrape the [Minnesota Early Childhood
Longitudinal Data System website](http://eclds.mn.gov/).

## Installation

You can install the development version of eclds with:

``` r
remotes::install_github("hktosun/eclds")
```

## Usage

You need a Selenium Docker image running to use this package. Please
follow the instructions provided
[here](https://docs.ropensci.org/RSelenium/articles/docker.html).

To scrape the individuals sections in the website, use the function
[`scrape_eclds()`](../reference/scrape_eclds.html). The code below
scrapes the tables in the Early Care and Education subsection in
Kindergarten section for each county between the school years 2018 and
2020.

``` r
eclds::scrape_eclds(section = 'kindergarten', 
                    subsection = 'early care and education', 
                    geography = 'county', 
                    year = 2018:2020)
```
