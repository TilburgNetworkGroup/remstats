# baseline

Specifies an intercept for the tie-oriented model or the sender activity
rate step of the actor-oriented model in the same manner as in
[`lm`](https://rdrr.io/r/stats/lm.html) (see Details).

## Details

A baseline effect is automatically specified for the tie-oriented model
and the sender activity rate step of the actor-oriented model when the
`ordinal` argument in
[`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md),
[`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md),
[`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md)
is set to FALSE (default) and automatically removed when this argument
is set to TRUE. Alternatively, a baseline effect can be explicitly
specified by adding '1' to the equation or explicitly removed by adding
'-1' to the equation.

The baseline effect refers to the baseline tendency to interact. In the
tie-oriented model, the log-inverse of the estimated parameter
translates to the average number of observed events per time unit per
dyad. In the actor-oriented model, the log-inverse of the estimated
parameter translates to the average number of observed events per time
unit per actor. The statistic is equal to one for all dyads resp. actors
in the riskset at all timepoints.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
remstats(reh = reh_tie, tie_effects = ~1)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 1 statistics
#> > Statistics:
#>   >> 1: baseline

reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, sender_effects = ~1)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: baseline
```
