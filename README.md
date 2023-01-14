
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BehavioralDataAnalysis

<!-- badges: start -->

[![check-standard](https://github.com/BehavioralDataAnalysis/R_package/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/BehavioralDataAnalysis/R_package/actions/workflows/check-standard.yaml)
<!-- badges: end -->

\*\* WORK IN PROGRESS! Please forgive the mess until the package is
ready for release \*\*

The goal of BehavioralDataAnalysis is to provide functions to help you
analyze behavioral data, i.e., data that represents the behavior of
human beings such as customers and employees. In particular, I believe
that there are two aspects of behavioral data that are worth
emphasizing: - it doesn’t obey a normal distribution nearly as often as
we assume. It can be asymmetrical (skewed), fat-tailed, kurtotic,
present multiple peaks and what have you. - we’re generally interested
in understanding what causes a behavior, so that we can affect it–e.g.,
increase customer spending or reduce employee churn. This requires the
use of experimental or quasi-experimental methods, many of which make
our data even less “well-behaved”, statistically speaking.

Both of these aspects call for dedicated analytical approaches, which is
what this package is about. I describe in more details this
“Causal-Behavioral Framework”, as I call it, in my book [Behavioral Data
Analysis with R and
Python](https://smile.amazon.com/Behavioral-Data-Analysis-Python-Customer-Driven-ebook/dp/B0979QYPWD/)
(O’Reilly Media). But you can totally use this package without reading
the book, and I’ve tried to make the documentation self-sustaining.

## Installation

You can install the development version of BehavioralDataAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BehavioralDataAnalysis/R_package")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BehavioralDataAnalysis)
## basic example code
```
