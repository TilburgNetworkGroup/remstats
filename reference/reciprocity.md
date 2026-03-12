# reciprocity

Specifies the statistic for a reciprocity effect in the tie-oriented
model or the receiver choice step of the actor-oriented model.

## Usage

``` r
reciprocity(scaling = c("none", "prop", "std"), consider_type = TRUE)
```

## Arguments

- scaling:

  the method for scaling the reciprocity statistic. Default is to not
  scale the statistic but keep the raw 'counts'. Alternatively, the
  statistics can be scaled by 'prop', in which raw counts are divided by
  the indegree of the sender at time t (see 'details') or
  standardization of the raw counts per time point can be requested with
  'std'.

- consider_type:

  logical, indicates whether to count the number of past reciprocal
  events separately for each event type (TRUE, default) or sum across
  different event types (FALSE).

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

A reciprocity effect refers to the tendency for actors to reciprocate
past interactions. The statistic at timepoint *t* for dyad *(i,j)*
(tie-oriented model) or receiver *j* (actor-oriented model) is equal to
the number of *(j,i)* events before timepoint *t*. Note that a
reciprocity effect is only defined for directed events.

Optionally, a scaling method can be set with `scaling`. By scaling the
reciprocity count by the indegree of the sender, the statistic refers to
the fraction of messages received by actor i that were received from
actor j. If actor i hasn't received any messages yet it can be assumed
that actor i is equally likely to receive a message from every actor and
the statistic is set equal to 1/(n-1), where n refers to the number of
actors. The resulting statistic is similar to the "FrRecSnd" statistic
in the R package 'relevent'.

## Examples

``` r
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ reciprocity()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: reciprocity

reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, receiver_effects = effects)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model: empty
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: reciprocity
```
