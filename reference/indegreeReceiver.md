# indegreeReceiver

Specifies the statistic for an \`indegreeReceiver\` effect in the
tie-oriented model or the receiver choice step of the actor-oriented
model.

## Usage

``` r
indegreeReceiver(scaling = c("none", "prop", "std"), consider_type = TRUE)
```

## Arguments

- scaling:

  the method for scaling the degree statistic. Default is to not scale
  the statistic (scaling = "none"). Alternatively, scaling of the raw
  degree counts by the number of past events at time t can be requested
  with 'prop' or standardization of the raw degree counts per time point
  can be requested with 'std'.

- consider_type:

  logical, indicates whether to count the degrees separately for each
  event type (TRUE, default) or sum degrees across different event types
  (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

An indegree of the receiver effect refers to the tendency for actors to
receive events if they have received more past events. The statistic at
timepoint *t* for dyad *(i,j)* (tie-oriented model) or receiver *j*
(actor-oriented model) is equal to the number of events received by
actor *j* before timepoint *t*. Note that the 'indegreeReceiver' effect
is only defined for directed events.

Optionally, a scaling method can be set with `scaling`. By scaling the
degree count by the total number of past events, the statistic refers to
the fraction of past events that were received by actor j. At the first
time point, when no events did previously occur, it is assumed that
every actor is equally likely to receive a message and the statistic is
set equal to 1/ n, where n refers to the number of actors.

## See also

[`indegreeSender`](https://tilburgnetworkgroup.github.io/remstats/reference/indegreeSender.md),
[`outdegreeSender`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeSender.md),
[`outdegreeReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeReceiver.md),
[`totaldegreeSender`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeSender.md),
or
[`totaldegreeReceiver`](https://tilburgnetworkgroup.github.io/remstats/reference/totaldegreeReceiver.md)
for other types of degree effects.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ indegreeReceiver()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: indegreeReceiver

reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, receiver_effects = effects)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model: empty
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: indegreeReceiver
```
