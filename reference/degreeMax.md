# degreeMax

Specifies the statistic for an \`degreeMax\` effect in the tie-oriented
model with undirected events.

## Usage

``` r
degreeMax(scaling = c("none", "prop", "std"), consider_type = TRUE)
```

## Arguments

- scaling:

  the method for scaling the degree statistic. Default is to not scale
  the statistic (scaling = "none"). Alternatively, scaling of the raw
  degree counts by two times the number of past events at time t can be
  requested with 'prop' or standardization of the raw degree counts per
  time point can be requested with 'std'.

- consider_type:

  logical, indicates whether to count the degrees separately for each
  event type (TRUE, default) or sum degrees across different event types
  (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

An degreeMax effect refers to the tendency for dyads to increase their
interaction rate if the total degree of the most active actor in the
pair increases. The statistic at timepoint *t* for dyad *(i,j)* is equal
to the maximum of the following two values: the number of events before
timepoint *t* that involved actor *i* and actor *j*, respectively. Note
that the degreeMax effect is only defined for undirected events.

Optionally, a scaling method can be set with `scaling`. By scaling the
degree count by the total number of past events, the statistic refers to
the fraction of past events that the most active actor was involved in.
At the first time point, when no events did previously occur, it is
assumed that every actor is equally likely to be involved in an event
and the statistic is set equal to 1/n, where n refers to the number of
actors.

## See also

[`degreeDiff`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeDiff.md),
[`degreeMin`](https://tilburgnetworkgroup.github.io/remstats/reference/degreeMin.md)
or
[`totaldegreeDyad`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeDyad.md)
for other types of degree effects for undirected events.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
effects <- ~ degreeMax()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 45 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: degreeMax
```
