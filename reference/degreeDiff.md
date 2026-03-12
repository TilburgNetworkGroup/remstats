# degreeDiff

Specifies the statistic for a \`degreeDiff\` effect in the tie-oriented
model.

## Usage

``` r
degreeDiff(scaling = c("none", "std"), consider_type = TRUE)
```

## Arguments

- scaling:

  the method for scaling the degree statistic. Default is to not scale
  the statistic (scaling = "none"). Alternatively, standardization of
  the degree difference per time point can be requested with \`std\`.

- consider_type:

  logical, indicates whether to count the degrees separately for each
  event type (TRUE, default) or sum degrees across different event types
  (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

A degreeDiff effect refers to the tendency for dyads to increase their
interaction rate if the absolute difference in degree for the two actors
in the pair increases. The statistic at timepoint *t* for dyad *(i,j)*
is equal to the difference between the following two values: the number
of events before timepoint *t* that involved actor *i* and actor *j*,
respectively. The degreeDiff effect is only defined for undirected
events.

## See also

[`degreeMin`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeMin.md),
[`degreeMax`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeMax.md)
or
[`totaldegreeDyad`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeDyad.md)
for other types of degree effects for undirected events.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
effects <- ~ degreeDiff()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 45 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: degreeDiff
```
