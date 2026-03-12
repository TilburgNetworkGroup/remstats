# psABXY

Specifies the statistic for a participation shift AB-XY.

## Usage

``` r
psABXY(consider_type = TRUE)
```

## Arguments

- consider_type:

  logical, indicates whether to consider the event type in determining
  which dyads create a pshift (TRUE, default) or not (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The AB-XY participation shift refers to one of Gibson's (2003) dyadic
participation shifts. The AB-XY pshift refers to a tendency for turn
usurping (here, the next sender and the next receiver are not in the
previous event). For each timepoint t, the psABXY statistic is equal to
one for the dyads that will create the participation shift if they would
occur in the edgelist at time t and equal to zero for the dyads that
will not create this participation shift. If consider_type is set to
TRUE, the type of the AB event and the type of the XY events have to be
equal. If it is set to FALSE, the participation shift is set to one for
every XY event, regardless of the event type. If multiple events in the
edgelist occur at the same time point, the order of these events
determines whether the p-shift is observed. Note that the AB-XY pshift
is only defined for directed events.

## See also

[`psABBA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBA.md),
[`psABBY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBY.md),
[`psABXA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXA.md),
[`psABXB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXB.md)
or
[`psABAY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAY.md)
for other dyadic participation shifts.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ psABXY()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: psABXY
```
