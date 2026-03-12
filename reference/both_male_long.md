# Exogenous Dyad Attribute in Long Format: both_male_long

A data frame representing exogenous attributes of dyads in a social
network in long format. Each row indicates whether a dyad consists of
two male actors (sex=0) in the original matrix \`info_both_male_wide\`.

## Usage

``` r
data(both_male_long)
```

## Format

A data frame with the following columns:

- actor1:

  Numeric id of the first actor in the dyad.

- actor2:

  Numeric id of the second actor in the dyad.

- both_male:

  Binary indicator (1 for male-male dyads, 0 otherwise).

## Source

Simulated exogenous information on actors in a social network.

## See also

[`tie`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md)
for the function using this data,
[`both_male_wide`](https://tilburgnetworkgroup.github.io/remstats/reference/both_male_wide.md)
for the data in wide format, and
[`info`](https://tilburgnetworkgroup.github.io/remstats/reference/info.md)
for an overview of the actor exogenous attributes.

## Examples

``` r
data(both_male_long)
head(both_male_long)
#>   actor1 actor2 both_male
#> 1    101    101         1
#> 2    101    103         1
#> 3    101    104         1
#> 4    101    105         1
#> 5    101    107         1
#> 6    101    109         1
```
