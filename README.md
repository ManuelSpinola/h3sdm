
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/h3sdm_logo_a.png"
style="float:left; margin-right:20px;" width="145" />

# h3sdm

**h3sdm**: Machine learning–based spatial species distribution modeling
and habitat/landscape analysis using H3 spatial index grids.

## Overview

**h3sdm** is an R package for **species distribution modeling (SDM)**
and **habitat analysis** using hexagonal spatial index grids based on
[H3](https://h3geo.org/).

It provides a consistent spatial framework to combine species occurrence
data with environmental predictors and landscape metrics, enabling both
ecological modeling and habitat characterization. The modeling framework
is built on **tidymodels**, offering flexibility to use different
approaches (e.g., logistic regression, GAMs, Random Forest, XGBoost).

Key features include:

- Conversion of point occurrence data into **H3-based spatial grids**
- Extraction of environmental and landscape predictors at different
  resolutions
- Support for multiple modeling approaches through **tidymodels**
- Tools for visualizing model predictions and habitat structure

By leveraging **H3 grids** and the **tidymodels** ecosystem, **h3sdm**
makes it easy to bridge **species distribution modeling** and
**landscape ecology** in a scalable way.

------------------------------------------------------------------------

## Installation

You can install the development version of **h3sdm from**
[GitHub](https://github.com/) using one of the following methods:

``` r
# install.packages("pak")
pak::pak("ManuelSpinola/h3sdm")
```

``` r
# install.packages("remotes")
remotes::install_github("ManuelSpinola/h3sdm")
```

``` r
# install.packages("devtools")
devtools::install_github("ManuelSpinola/h3sdm")
```

## Get Started

Explore **h3sdm** through the [Get Started
section](https://manuelspinola.github.io/h3sdm/) of the website, which
provides a quick introduction to the package and its core workflows.  
For more practical examples, visit the [Articles
section](https://manuelspinola.github.io/h3sdm/articles/), where each
vignette demonstrates different aspects of species distribution and
habitat analysis using H3 grids.
