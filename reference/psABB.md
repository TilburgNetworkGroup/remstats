# psABB

Specifies the statistic for a participation shift AB-B in the sender
step of the actor-oriented model.

## Usage

``` r
psABB()
```

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The AB-B participation shift refers to the tendency for immediate
reciprocation (the next sender is the previous receiver). For each
timepoint t, the psABBA statistic is equal to one for the actor (i.e,
the previous event receiver) that will create the participation shift if
it would occur as sender in the edgelist at time t and equal to zero for
the actors that will not create this participation shift. If multiple
events in the edgelist occur at the same time point, the order of these
events determines whether the p-shift is observed.

## See also

[`psABA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABA.md)
or
[`psABX`](https://tilburgnetworkgroup.github.io/remstats/reference/psABX.md)
for exploring alternative participation shifts in the sender step of the
actor-oriented model.

## Examples

``` r
reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, sender_effects = ~ psABB())
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: psABB
```
