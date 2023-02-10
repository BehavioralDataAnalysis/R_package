
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BehavioralDataAnalysis

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/BehavioralDataAnalysis/R_package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/BehavioralDataAnalysis/R_package?branch=main)
[![check-standard](https://github.com/BehavioralDataAnalysis/R_package/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/BehavioralDataAnalysis/R_package/actions/workflows/check-standard.yaml)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

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

Please note that the package is designed to integrate nicely with the
Tidyverse and therefore most functions will expect data formatted as a
data.frame or a tibble.

## Installation

You can install the development version of BehavioralDataAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BehavioralDataAnalysis/R_package")
```

## Examples

### Bootstrap confidence interval

The function that you’re most likely to use is probably `boot_ci()`,
which estimates a Bootstrap interval for a function applied to a
dataset. While the `boot.ci()` function of the [boot
package]('https://cran.r-project.org/web/packages/boot/index.html')
offers more options and is more powerful, it often requires more memory
and computation than my personal laptop can manage and I find it
somewhat cumbersome to use. Definitely check it out if you need a more
serious implementation than the one here!

You can pass to `boot_ci()` any function that takes as argument a data
frame and returns a single number:

``` r
library(BehavioralDataAnalysis)
my_data <- data.frame(
  x = rnorm(100)
)

my_function <- function(df) { return(mean(df$x)) }

CI <- boot_ci(my_data, my_function)
print(CI)
#> [1] -0.1550445  0.2011823
```

However, the most common use case is probably to use it to run a
regression, so you can also pass directly the formula for a linear
regression as the second parameter. For example, let’s see what is the
relationship between mass and height in the `starwars` dataset.

``` r
data(starwars, package = "dplyr")

CI <- boot_ci(starwars, 'mass~height')
print(CI)
#>             lower_bound upper_bound
#> (Intercept)  -48.179736  59.1117148
#> height         0.384667   0.7584546
```

### matching subject for experimentation

If you have access to your whole list of subjects ahead of time (e.g.,
as opposed to users visiting at random your website), you can pair
subjects sharing similar characteristics, to ensure that your
experimental groups are as balanced as possible. This is also called
stratified assignment, hence the name of the function `paired_assign()`.
Note however that it will make traditional statistics invalid, and
you’ll have to use the Bootstrap to build intervals around your central
estimates.

``` r
library(dplyr)
library(BehavioralDataAnalysis)
attach(starwars)
set.seed(1)
dat <- starwars %>%
  na.omit() %>%
  dplyr::select(-films, -vehicles, -starships) %>%
  dplyr::filter(!grepl('Dooku', name))

paired_assigned_dat <- paired_assign(dat, id = 'name')
summ <- paired_assigned_dat %>% 
  group_by(grp) %>% 
  summarize(mean_height = mean(height, na.rm = TRUE))
print(summ)
#> # A tibble: 2 × 2
#>     grp mean_height
#>   <dbl>       <dbl>
#> 1     0        180.
#> 2     1        175.
```

As we can see, the mean heights of the two groups are pretty close. With
pure randomization on the other hand, the two values are further apart
from each other:

``` r
set.seed(1)
rnd_dat <- dat %>%
  mutate(grp = c(rep(0, 14), rep(1, 14))) %>%
  mutate(grp = sample(grp))
rnd_summ <- rnd_dat %>% 
  group_by(grp) %>% 
  summarize(mean_height = mean(height, na.rm = TRUE))
print(rnd_summ)
#> # A tibble: 2 × 2
#>     grp mean_height
#>   <dbl>       <dbl>
#> 1     0        171.
#> 2     1        184.
```
