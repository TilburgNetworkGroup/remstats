# psABA

Specifies the statistic for a participation shift AB-A in the sender
step of the actor-oriented model.

## Usage

``` r
psABA()
```

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

Refers to the tendency for the same actor to keep initiating events: The
next sender is equal to the previous sender. For each timepoint t, the
psABA statistic is equal to one for the actor that will create the
participation shift if they would occur in the edgelist as the sender at
time t and equal to zero for the actors that will not create this
participation shift. If multiple events in the edgelist occur at the
same time point, the order of these events determines whether the
p-shift is observed.

## See also

[`psABB`](https://tilburgnetworkgroup.github.io/remstats/reference/psABB.md)
or
[`psABX`](https://tilburgnetworkgroup.github.io/remstats/reference/psABX.md)
for exploring alternative participation shifts in the sender step of the
actor-oriented model.

## Examples

``` r
reh_actor <- remify::remify(history, model = "actor")
remstats(sender_effects = ~ psABA(), reh = reh_actor)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: psABA
```
