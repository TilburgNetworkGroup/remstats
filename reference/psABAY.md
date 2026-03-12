# psABAY

Specifies the statistic for a participation shift AB-AY.

## Usage

``` r
psABAY(consider_type = TRUE)
```

## Arguments

- consider_type:

  logical, indicates whether to consider the event type in determining
  which dyads create a pshift (TRUE, default) or not (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

One of Gibson's (2003) dyadic participation shifts. The AB-AY
participation shift refers to a tendency for *turn continuing*. For
directed events, the sender (A) in the current event is the same as the
sender in the previous event (A), and the receiver (Y) is different from
the previous receiver (B). In undirected events, one of the current
actors (A) matches one of the actors in the previous events (A or B),
while the other actor (Y) is different.

To identify these shifts, a statistic 'psABAY' is calculated for each
pair of actors at a given timepoint (t). If the pair's interaction
follows the AB-AY pattern, the statistic is set equal to one; otherwise,
it's set to zero.

Additionally, the types of the AB and AY events can be taken into
account. If 'consider_type' is 'TRUE', the type of the AB event and the
type of the AY event must match for the shift to occur. If
'consider_type' is 'FALSE', the shift happens for every AY event,
regardless of the event type.

## See also

[`psABBA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBA.md),
[`psABBY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABBY.md),
[`psABXA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXA.md),
[`psABXB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXB.md),
[`psABXY`](https://tilburgnetworkgroup.github.io/remstats/reference/psABXY.md)
or
[`psABAB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABAB.md)
for other dyadic participation shifts.

## Examples

``` r
reh <- remify::remify(history, model = "tie")
effects <- ~ psABAY()
remstats(reh = reh, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: psABAY
```
