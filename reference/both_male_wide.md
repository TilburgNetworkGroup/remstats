# Exogenous Dyad Attribute Matrix: both_male_wide

A matrix representing exogenous attributes of dyads in a social network.
The matrix indicates whether a dyad consists of two male actors (sex=0).
Rows and columns correspond to actor IDs, and cells contain binary
values (1 for male-male dyads, 0 otherwise).

## Usage

``` r
data(both_male_wide)
```

## Format

A square matrix with dimensions equal to the number of unique actors.

## Source

Simulated exogenous information on actors in a social network.

## See also

[`tie`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md)
for the function using this data,
[`both_male_long`](https://tilburgnetworkgroup.github.io/remstats/reference/both_male_long.md)
for the data in long format, and
[`info`](https://tilburgnetworkgroup.github.io/remstats/reference/info.md)
for an overview of the actor exogenous attributes.

## Examples

``` r
data(both_male_wide)
print(both_male_wide)
#>     101 103 104 105 107 109 111 112 113 115
#> 101   1   1   1   1   1   1   0   1   1   1
#> 103   1   1   1   1   1   1   0   1   1   1
#> 104   1   1   1   1   1   1   0   1   1   1
#> 105   1   1   1   1   1   1   0   1   1   1
#> 107   1   1   1   1   1   1   0   1   1   1
#> 109   1   1   1   1   1   1   0   1   1   1
#> 111   0   0   0   0   0   0   0   0   0   0
#> 112   1   1   1   1   1   1   0   1   1   1
#> 113   1   1   1   1   1   1   0   1   1   1
#> 115   1   1   1   1   1   1   0   1   1   1
```
