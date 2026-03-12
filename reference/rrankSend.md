# rrankSend

Specifies the statistic for a recency rank send effect in the `effects`
argument of
[`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md)
or the `receiver_effects` argument of
[`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md).

## Usage

``` r
rrankSend(consider_type = TRUE)
```

## Arguments

- consider_type:

  logical, indicates whether to discriminate between event types in
  determining the event rank (TRUE, default) or not (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The rrankSend effect refers to a rank-based recency effect, as described
in section 2.2.5 of Butts (2008). For each timepoint t, for directed
dyad (i,j) the statistic is equal to the inverse of the rank of receiver
j among the actors to which sender i has most recently send past events.
Note that the 'rrankSend' effect is only defined for directed events.

## See also

[`rrankReceive`](https://tilburgnetworkgroup.github.io/remstats/reference/rrankReceive.md),
[`recencySendSender`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendSender.md),
[`recencySendReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/recencySendReceiver.md),
[`recencyReceiveSender`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveSender.md),
[`recencyReceiveReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyReceiveReceiver.md)
and
[`recencyContinue`](https://tilburgnetworkgroup.github.io/remstats/reference/recencyContinue.md)
for other type of recency effects

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ rrankSend()
remstats(tie_effects = effects, reh = reh_tie)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: rrankSend

reh_actor <- remify::remify(history, model = "actor")
remstats(receiver_effects = effects, reh = reh_actor)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model: empty
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: rrankSend
```
