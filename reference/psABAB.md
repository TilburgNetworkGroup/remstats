# psABAB

Specifies the statistic for a pshift AB-AB effect.

## Usage

``` r
psABAB(consider_type = TRUE)
```

## Arguments

- consider_type:

  logical, indicates whether to consider the event type in determining
  which dyads create a pshift (TRUE, default) or not (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

Refers to the tendency for the same dyads to keep interacting. For
directed events, the next sender and receiver are equal to the previous
sender and receiver. For undirected events, the next actor pair is equal
to the current actor pair. For each timepoint t, the psABAB statistic is
equal to one for the dyads that will create the participation shift if
they would occur in the edgelist at time t and equal to zero for the
dyads that will not create this participation shift. If consider_type is
set to TRUE, the type of the two subsequent AB events have to be equal.
If it is set to FALSE, the participation shift is set to one for every
AB event, regardless of the event type. If multiple events in the
edgelist occur at the same time point, the order of these events
determines whether the p-shift is observed.

## See also

[`psABBA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBA.md),
[`psABBY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBY.md),
[`psABXA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXA.md),
[`psABXB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXB.md),
[`psABXY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXY.md)
or
[`psABAY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAY.md)
for other dyadic participation shifts.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
effects <- ~ psABAB()
remstats(tie_effects = effects, reh = reh_tie)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 45 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: psABAB
```
