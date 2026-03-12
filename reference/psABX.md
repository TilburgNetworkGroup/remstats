# psABX

Specifies the statistic for a participation shift AB-X in the sender
step of the actor-oriented model.

## Usage

``` r
psABX()
```

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The AB-X participation shift refers to a tendency for turn usurping
(here, the next sender is not in the previous event). For each timepoint
t, the psABX statistic is equal to one for the actors that will create
the participation shift if they would occur as the sender in the
edgelist at time t and equal to zero for the actors that will not create
this participation shift. If multiple events in the edgelist occur at
the same time point, the order of these events determines whether the
p-shift is observed.

## See also

[`psABA`](https://tilburgnetworkgroup.github.io/remstats/reference/psABA.md)
or
[`psABB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABB.md)
for exploring alternative participation shifts in the sender step of the
actor-oriented model.

## Examples

``` r
reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, sender_effects = ~ psABX())
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: psABX
```
