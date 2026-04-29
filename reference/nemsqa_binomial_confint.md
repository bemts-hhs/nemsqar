# Wilson and Clopper-Pearson Confidence Intervals for Binomial Proportions

Computes confidence intervals for binomial proportions using either the
Wilson or Clopper-Pearson method. This function supports vectorized
operations and allows optional correction for continuity. The Wilson
interval is computed using
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html), while
the Clopper-Pearson interval is computed using
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html).

## Usage

``` r
nemsqa_binomial_confint(
  data = NULL,
  x,
  n,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE
)
```

## Arguments

- data:

  An optional `tibble` or `data.frame` containing the variables `x` and
  `n`. If provided, `x` and `n` should be column names.

- x:

  Numeric vector or column name (if `data` is provided) representing the
  number of successes.

- n:

  Numeric vector or column name (if `data` is provided) representing the
  total number of trials.

- method:

  Character string specifying the confidence interval method. Must be
  either "wilson" (default) or "clopper-pearson".

- conf.level:

  Numeric value between 0 and 1 indicating the confidence level.
  Defaults to 0.95 (95% confidence interval).

- correct:

  Logical, indicating whether to apply continuity correction for Wilson
  intervals. Defaults to `TRUE`.

## Value

A `tibble` containing the estimated proportion (`prop`), lower
confidence interval (`lower_ci`), upper confidence interval
(`upper_ci`), and a formatted proportion label (`prop_label`). If `data`
is provided, these columns are appended to `data` via
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html).

## Details

The Wilson confidence interval is calculated using
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html), which
provides an improved approximation to the binomial proportion confidence
interval by avoiding the instability of the Wald interval (Wilson,
1927). The Clopper-Pearson interval, computed using
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html), is an
exact method based on the cumulative probabilities of the binomial
distribution (Clopper & Pearson, 1934).

The use of [`match.arg()`](https://rdrr.io/r/base/match.arg.html) within
`nemsqar::nemsqa_binomial_confint()` allows users to specify the method
using partial matching, meaning they can enter just "w" instead of
"wilson" or "c" instead of "clopper-pearson".

## References

Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or
fiducial limits illustrated in the case of the binomial. Biometrika, 26,
404–413. [doi:10.2307/2331986](https://doi.org/10.2307/2331986) .

Wilson, E.B. (1927). Probable inference, the law of succession, and
statistical inference. Journal of the American Statistical Association,
22, 209–212. [doi:10.2307/2276774](https://doi.org/10.2307/2276774) .

## Examples

``` r
# Example without a data frame
nemsqa_binomial_confint(data = NULL,
                        x = c(5, 10, 20),
                        n = c(50, 100, 200),
                        method = "wilson"
                        )
#> # A tibble: 3 × 4
#>    prop prop_label lower_ci upper_ci
#>   <dbl> <chr>         <dbl>    <dbl>
#> 1   0.1 10%          0.0374    0.226
#> 2   0.1 10%          0.0516    0.180
#> 3   0.1 10%          0.0637    0.152

# Example with a data.frame
data <- data.frame(successes = c(5, 10, 20), trials = c(50, 100, 200))
nemsqa_binomial_confint(data,
                        x = successes,
                        n = trials,
                        method = "clopper-pearson"
                        )
#>   successes trials prop prop_label   lower_ci  upper_ci
#> 1         5     50  0.1        10% 0.03327509 0.2181354
#> 2        10    100  0.1        10% 0.04900469 0.1762226
#> 3        20    200  0.1        10% 0.06215937 0.1502128
```
