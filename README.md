
<!-- README.md is generated from README.Rmd. Please edit that file -->

# catalogoUCsBR

<!-- badges: start -->

[![R-CMD-check](https://github.com/p/catalogoUCsBR/pablopains/R-CMD-check/badge.svg)](https://github.com/pablopains/catalogoUCsBR/actions)
[![Codecov test
coverage](https://codecov.io/gh/pablopains/catalogoUCsBR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pablopains/catalogoUCsBR?branch=main)
[![R-CMD-check](https://github.com/pablopains/catalogoUCsBR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pablopains/catalogoUCsBR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The **catalogoUCsBR** package is designed to convert herbarium species
occurrence data from JABOT, Reflora, speciesLink, and GBIF data portals
into a suitable format to prepare plant species lists revised for the
[Catalog of Plants of Conservation Units in
Brazil](https://catalogo-ucs-brasil.jbrj.gov.br/). The package provides
tools to verify and standardize scientific names of species, join
duplicates, and select species records as a voucher to compose lists of
plant species in UCs in Brazil, helping to understand and protect the
biodiversity in protected areas.

## Installation

You can install the development version of catalogoUCsBR from
[GitHub](https://github.com/pablopains/catalogoUCsBR). To install
catalogoUCsBR, run

``` r
install.packages('remotes',  dependencies = T)

remotes::install_github("pablopains/catalogoUCsBR",
                        dependencies = T)
```

# **Botanical experts**

## **Use the prepare app locally in RStudio**

``` r
catalogoUCsBR::app_prepare()
```

## **Use the review app locally in RStudio**

``` r
catalogoUCsBR::app_review()
```

## **Consult the [catalogoUCsBR Manual](https://github.com/pablopains/catalogoUCsBR/blob/main/catalogoUCsBR_1.0.4.pdf) for a case study with a complete and replicable workflow**

Please site catalogoUCsBR as:

``` r
print(citation("catalogoUCsBR"), bibtex = FALSE)
#> Para citar o pacote 'catalogoUCsBR' em publicações use:
#> 
#>   Melo P, Bochorny T, Forzza R (2024). _catalogoUCsBR: An R package to
#>   prepare species lists for the Catalog of Plants of Conservation Units
#>   in Brazil._. R package version 1.0.0, commit
#>   2cfbf4e7d21b8de4b88a21e6d6fd02234464e33e,
#>   <https://github.com/pablopains/catalogoUCsBR>.
```
