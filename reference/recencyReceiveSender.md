# recencyReceiveSender

Specifies the statistic for a recency receive of sender effect in the
`effects` argument of
[`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md)
or the `sender_effects` argument of
[`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md).

## Usage

``` r
recencyReceiveSender(consider_type = TRUE)
```

## Arguments

- consider_type:

  logical, indicates whether to compute the recency separately for each
  event type (TRUE, default) or regardless of event types (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The recencyReceiveSender effect refers to a recency statistic similar to
what is described in Vu et al. (2017) and Mulder and Leenders (2019).
For each timepoint t, for directed dyad (i,j) the statistic is equal to
1/(the time that has past since sender i was last active as receiver +
1). Note that the 'recencyReceiveSender' effect is only defined for
directed events.

## See also

[`rrankSend`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankSend.md),
[`rrankReceive`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankReceive.md),
[`recencySendSender`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendSender.md),
[`recencySendReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendReceiver.md),
[`recencyReceiveReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveReceiver.md)
and
[`recencyContinue`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyContinue.md)
for other type of recency effects

## Examples

``` r
effects <- ~ recencyReceiveSender()
reh_tie <- remify::remify(history, model = "tie")
remstats(tie_effects = effects, reh = reh_tie)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: recencyReceiveSender

reh_actor <- remify::remify(history, model = "actor")
remstats(sender_effects = effects, reh = reh_actor)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: recencyReceiveSender
```
