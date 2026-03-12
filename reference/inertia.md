# inertia

Specifies the statistic for an inertia effect in the tie-oriented model
or the receiver choice step of the actor-oriented model.

## Usage

``` r
inertia(scaling = c("none", "prop", "std"), consider_type = TRUE)
```

## Arguments

- scaling:

  the method for scaling the inertia statistic. Default is to not scale
  the statistic (scaling = "none"). Alternatively, the statistics can be
  scaled by specifying 'prop', in which raw counts are divided by the
  outdegree of the sender at time t (see 'details') or standardization
  of the raw counts per time point can be requested with 'std'.

- consider_type:

  logical, indicates whether to count the number of past events
  separately for each event type (TRUE, default) or sum across different
  event types (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

An inertia effect refers to the tendency for dyads to repeatedly
interact with each other (tie-oriented model) or for actors to
repeatedly choose the same actor as receiver of their events
(actor-oriented model). The statistic at timepoint *t* for dyad *(i,j)*
resp. receiver *j* is equal to the number of *(i,j)* events before
timepoint *t*.

Optionally, a scaling method can be set with `scaling`. By scaling the
inertia count by the outdegree of the sender ("prop"), the statistic
refers to the fraction of messages send by actor i that were send to
actor j. If actor i hasn't send any messages yet it can be assumed that
every actor is equally likely to receive a message from i and the
statistic is set equal to 1/(n-1), where n refers to the number of
actors. The resulting statistic is similar to the "FrPSndSnd" statistic
in the R package 'relevent', or the persistence statistic in Section
2.2.2 of Butts (2008). Note that this scaling method is only defined for
directed events.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ inertia()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: inertia

reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, receiver_effects = effects)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model: empty
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: inertia
```
