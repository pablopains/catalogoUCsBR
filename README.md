
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
devtools::install_github("pablopains/catalogoUCsBR", dependencies = TRUE)
```

# **Botanical experts**

## **Use the prepare app locally in RStudio**

``` r
catalogoUCsBR::app_prepare()
```
## **Use the prepare app online**

[prepare app on-line](https://pablopains.shinyapps.io/catalogoUCsBR_prepare/)

## **Use the review app locally in RStudio**

``` r
catalogoUCsBR::app_review()
```
## **Use the review app on-line**

[review app on-line](https://pablopains.shinyapps.io/catalogoUCsBR_review/)


## **Use the publication app locally in RStudio**

``` r
catalogoUCsBR::app_review()
```
## **Use the publication app on-line**

[publication app on-line](https://pablopains.shinyapps.io/catalogoUCsBR_publication/)


## **Consult the [Instruction manual for data download](https://github.com/pablopains/catalogoUCsBR/blob/main/Manual%20de%20instru%C3%A7%C3%B5es%20para%20download_CatalogoUCsBR.pdf)**


## **Consult the [catalogoUCsBR Manual](https://github.com/pablopains/catalogoUCsBR/blob/main/catalogoUCsBR_1.0.0.pdf) for a case study with a complete and replicable workflow**



Please site catalogoUCsBR as:

``` r
print(citation("catalogoUCsBR"), bibtex = FALSE)
#> To cite package 'catalogoUCsBR' in publications use:
#> 
#>   Melo P, Bochorny T, Forzza R (2023). _catalogoUCsBR: An R package for
#>   preparing species listings for the Plant Catalog of Units of
#>   Brazilian Conservation._. R package version 1.0.0.
```
