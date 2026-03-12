# sp

Specifies the statistic for a shared partners effect for undirected
events.

## Usage

``` r
sp(unique = FALSE, scaling = c("none", "std"), consider_type = TRUE)
```

## Arguments

- unique:

  A logical value indicating whether to sum the minimum of events with
  third actors (FALSE, default) or the number of third actors that
  create a new, unique shared partner (TRUE). See details for more
  information.

- scaling:

  the method for scaling the triad statistic. Default is to not scale
  the statistic but keep the raw 'counts'. Alternatively,
  standardization of the raw counts per time point can be requested with
  'std'.

- consider_type:

  logical, indicates whether to count the shared partners separately for
  each event type (TRUE, default) or sum across different event types
  (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The shared partners effect describes the propensity of dyads to interact
based on the number of past shared partners between them. By default,
the statistic at timepoint t for the undirected dyad (i,j) is computed
as the sum of the minimum occurrences of past undirected (i,h) and
undirected (j,h) events across all actors h.

When the unique parameter is set to TRUE, a different approach is taken.
In this case, the statistic counts the number of actors h that
contribute to the creation of a new, distinct shared partner between
actors i and j.

Additionally, it is possible to specify a scaling method using the
scaling parameter.

Please note that the shared partners effect, 'sp', is exclusively
defined for undirected events.

## See also

[`otp`](https://tilburgnetworkgroup.github.io/remstats/reference/otp.md),
[`itp`](https://tilburgnetworkgroup.github.io/remstats/reference/itp.md),
[`osp`](https://tilburgnetworkgroup.github.io/remstats/reference/osp.md),
or
[`isp`](https://tilburgnetworkgroup.github.io/remstats/reference/isp.md)
for triadic effects for directed relational events.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
effects <- ~ sp()
remstats(tie_effects = effects, reh = reh_tie)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 45 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: sp
```
