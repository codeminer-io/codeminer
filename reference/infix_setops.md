# Infix set operations

Infix versions of
[dplyr::setops](https://dplyr.tidyverse.org/reference/setops.html).
`%AND%`, `%OR%` and `%NOT%` are the equivalents of
[`dplyr::intersect()`](https://generics.r-lib.org/reference/setops.html),
[`dplyr::union()`](https://generics.r-lib.org/reference/setops.html) and
[`dplyr::setdiff()`](https://generics.r-lib.org/reference/setops.html)
respectively.

## Usage

``` r
x %AND% y

x %OR% y

x %NOT% y
```

## Arguments

- x, y:

  Pair of compatible data frames. A pair of data frames is compatible if
  they have the same column names (possibly in different orders) and
  compatible types.

## Examples

``` r
df1 <- tibble::tibble(x = 1:3)
df2 <- tibble::tibble(x = 3:5)

# equivalent of dplyr::intersect(df1, df2)
df1 %AND% df2
#> # A tibble: 1 × 1
#>       x
#>   <int>
#> 1     3

# equivalent of dplyr::union(df1, df2)
df1 %OR% df2
#> # A tibble: 5 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5

# equivalent of dplyr::setdiff(df1, df2). Note that argument order matters
df1 %NOT% df2
#> # A tibble: 2 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
df2 %NOT% df1
#> # A tibble: 2 × 1
#>       x
#>   <int>
#> 1     4
#> 2     5
```
