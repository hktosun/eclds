
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eclds

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
`scrape_eclds()`. The code below scrapes the tables in the Early Care
and Education subsection in Kindergarten section for each county between
the school years 2018 and 2020.

``` r
eclds::scrape_eclds(section = 'kindergarten', 
                    subsection = 'early care and education', 
                    geography = 'county', 
                    year = 2018:2020)
```

Available sections and subsections are as follows:

| Section           | Subsection                       | Available Geographies           |
|-------------------|----------------------------------|---------------------------------|
| `"birth to prek"` | `"early childhood screening"`    | `"county"`, `"school district"` |
| `"birth to prek"` | `"scholarships"`                 | `"county"`, `"school district"` |
| `"birth to prek"` | `"parent aware"`                 | `"county"`                      |
| `"kindergarten"`  | `"early care and education"`     | `"county"`, `"school district"` |
| `"kindergarten"`  | `"child demographics"`           | `"county"`, `"school district"` |
| `"kindergarten"`  | `"family demographics"`          | `"county"`, `"school district"` |
| `"kindergarten"`  | `"economic and food assistance"` | `"county"`, `"school district"` |
| `"kindergarten"`  | `"kindergarten attendance"`      | `"county"`, `"school district"` |
| `"kindergarten"`  | `"kindergarten summary"`         | `"county"`, `"school district"` |

The available years are from `2014` to `2020` as of October 7, 2021.
