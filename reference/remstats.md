# remstats

Computes statistics for modeling relational events with a tie-oriented
or actor-oriented approach.

## Usage

``` r
remstats(
  reh,
  tie_effects = NULL,
  sender_effects = NULL,
  receiver_effects = NULL,
  attr_actors = NULL,
  attr_dyads = NULL,
  method = c("pt", "pe"),
  memory = c("full", "window", "decay", "interval"),
  memory_value = NA,
  start = 1,
  stop = Inf,
  display_progress = FALSE,
  adjmat = NULL,
  get_adjmat = FALSE,
  attr_data,
  attributes,
  edgelist
)
```

## Arguments

- reh:

  an object of class
  `"`[`remify`](https://tilburgnetworkgroup.github.io/remify/reference/remify.html)`"`
  characterizing the relational event history.

- tie_effects:

  an object of class
  `"`[`formula`](https://rdrr.io/r/stats/formula.html)`"` (or one that
  can be coerced to that class): a symbolic description of the effects
  in the tie-oriented model for which statistics are computed, see
  'Details' for the available effects and their corresponding statistics

- sender_effects:

  an object of class
  `"`[`formula`](https://rdrr.io/r/stats/formula.html)`"` (or one that
  can be coerced to that class): a symbolic description of the effects
  in the sender activity rate step of the actor-oriented model for which
  statistics are computed, see \`Details'

- receiver_effects:

  an object of class
  `"`[`formula`](https://rdrr.io/r/stats/formula.html)`"` (or one that
  can be coerced to that class): a symbolic description of the effects
  in the receiver choice step of model for which statistics are
  computed, see \`Details'

- attr_actors:

  optionally, an object of class
  `"`[`data.frame`](https://rdrr.io/r/base/data.frame.html)`"` that
  contains exogenous attributes for actors (see Details).

- attr_dyads:

  optionally, an object of class `data.frame` or `matrix` containing
  attribute information for dyads (see Details).

- method:

  Specifies the method for managing simultaneous events, i.e., events
  occurring at the same time. The default 'method' is 'pt' (per
  timepoint), where statistics are computed once for each unique
  timepoint in the edgelist. Alternatively, you can choose 'pe' (per
  event), where statistics are computed once for each unique event
  observed in the edgelist.

- memory:

  The memory to be used. See \`Details'.

- memory_value:

  Numeric value indicating the memory parameter. See \`Details'.

- start:

  an optional integer value, specifying the index of the first time or
  event in the relational event history for which statistics must be
  computed (see 'Details')

- stop:

  an optional integer value, specifying the index of the last time or
  event in the relational event history for which statistics must be
  computed (see 'Details')

- display_progress:

  should a progress bar for the computation of the endogenous statistics
  be shown (TRUE) or not (FALSE)?

- adjmat:

  optionally, for a tie-oriented model a previously computed adjacency
  matrix with on the rows the time points and on the columns the risk
  set entries

- get_adjmat:

  for a tie-oriented model, whether the adjmat computed by remstats
  should be outputted as an attribute of the statistics.

- attr_data:

  deprecated, please use "attr_actors" instead

- attributes:

  deprecated, please use "attr_data" instead

- edgelist:

  deprecated, please use "reh" instead

## Value

An object of class 'remstats'. In case of the tie-oriented model, an
array with the computed statistics, where rows refer to time points,
columns refer to potential relational event (i.e., potential edges) in
the risk set and slices refer to statistics. In case of the
actor-oriented model, list with in the first element the statistics for
the sender activity rate step and in the second element the statistics
for the receiver choice step, where rows refer to time points, columns
refer to potential senders or receivers, respectively. The 'remstats'
object has the following attributes:

- `model`:

  Type of model that is estimated, obtained from the remify object
  inputted to 'reh'.

- `formula`:

  Model formula, obtained from the formula inputted to 'tie_effects',
  'sender_effects' and/or 'receiver_effects', depending on the model.

- `riskset`:

  For the tie-oriented model, the risk set used to construct the
  statistics.

- `actors`:

  For the actor-oriented model, the set of actors used to construct the
  statistics, obtained from the remify object inputted to 'reh'.

- `adjmat`:

  \[Optional\], for the tie-oriented model, if "get_adjmat = TRUE", the
  matrix with the accumulated event weights for each time point (on the
  rows) and each dyad (in the columns).

## Effects

The statistics to be computed are defined symbolically and should be
supplied to the `tie_effects` (for the tie-oriented model), or
`sender_effects` and/or `receiver_effects` (for the actor-oriented
model) argument in the form `~ effects`. The terms are separated by +
operators. For example: `effects = ~ inertia() + otp()`. Interactions
between two effects can be included with \* or : operators. For example:
`effects = ~ inertia():otp()`. A list of available effects can be
obtained with
[`tie_effects()`](https://tilburgnetworkgroup.github.io/remstats/reference/tie_effects.md)
and
[`actor_effects()`](https://tilburgnetworkgroup.github.io/remstats/reference/actor_effects.md).

The majority of the statistics can be scaled in some way, see the
documentation of the `scaling` argument in the separate effect functions
for more information on this.

The majority of the statistics can account for the event type included
as a dependent variable, see the documentation of the `consider_type`
argument in the separate effect functions for more information on this.
Note that this option is only available for the tie-oriented model.

Note that events in the relational event history can be directed or
undirected. Some statistics are only defined for either directed or
undirected events (see the documentation of the statistics). Note that
undirected events are only available for the tie-oriented model.

## attr_actors

For the computation of the *exogenous* statistics an attributes object
with the exogenous covariate information has to be supplied to the
`attr_actors` argument in either `remstats()` or in the separate effect
functions supplied to the `..._effects` arguments (e.g., see
[`send`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md)).
This `attr_actors` object should be constructed as follows: A dataframe
with rows referring to the attribute value of actor *i* at timepoint
*t*. A \`name\` column is required that contains the actor name
(corresponding to the actor names in the relational event history). A
\`time\` column is required that contains the time when attributes
change (set to zero if none of the attributes vary over time).
Subsequent columns contain the attributes that are called in the
specifications of exogenous statistics (column name corresponding to the
string supplied to the `variable` argument in the effect function). Note
that the procedure for the exogenous effects \`tie' and \`event'
deviates from this, here the exogenous covariate information has to be
specified in a different way, see
[`tie`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md)
and
[`event`](https://tilburgnetworkgroup.github.io/remstats/reference/event.md).

## attr_dyads

For the computation of the *dyad exogenous* statistics with
[`tie()`](https://tilburgnetworkgroup.github.io/remstats/reference/tie.md),
an attributes object with the exogenous covariates information per dyad
has to be supplied. This is a `data.frame` or `matrix` containing
attribute information for dyads. If `attr_dyads` is a `data.frame`, the
first two columns should represent "actor1" and "actor2" (for directed
events, "actor1" corresponds to the sender, and "actor2" corresponds to
the receiver). Additional columns can represent dyads' exogenous
attributes. If attributes vary over time, include a column named "time".
If `attr_dyads` is a `matrix`, the rows correspond to "actor1", columns
to "actor2", and cells contain dyads' exogenous attributes.

## Memory

The default \`memory\` setting is \`"full"\`, which implies that at each
time point \$t\$ the entire event history before \$t\$ is included in
the computation of the statistics. Alternatively, when \`memory\` is set
to \`"window"\`, only the past event history within a given time window
is considered (see Mulders & Leenders, 2019). This length of this time
window is set by the \`memory_value\` parameter. For example, when
\`memory_value = 100\` and \`memory = "window"\`, at time point \$t\$
only the past events that happened at most 100 time units ago are
included in the computation of the statistics. A third option is to set
\`memory\` to \`"interval"\`. In this case, the past event history
within a given time interval is considered. For example, when
\`"memory_value" = c(50, 100)\` and \`memory = "window"\`, at time point
\$t\$ only the past events that happened between 50 and 100 time units
ago are included in the computation of the statistics. Finally, the
fourth option is to set \`memory\` to \`"decay"\`. In this case, the
weight of the past event in the computation of the statistics depend on
the elapsed time between \$t\$ and the past event. This weight is
determined based on an exponential decay function with half-life
parameter \`memory_value\` (see Brandes et al., 2009).

## Event weights

Note that if the relational event history contains a column that is
named “weight”, it is assumed that these affect the endogenous
statistics. These affect the computation of all endogenous statistics
with a few exceptions that follow logically from their definition (e.g.,
the recenyContinue statistic does depend on time since the event and not
on event weights).

## Subset the event history using 'start' and 'stop'

It is possible to compute statistics for a segment of the relational
event sequence, based on the entire event history. This is done by
specifying the 'start' and 'stop' values as the indices for the first
and last event times for which statistics are needed. For instance,
setting 'start = 5' and 'stop = 5' calculates statistics for the 5th
event in the relational event sequence, considering events 1-4 in the
history. Note that in cases of simultaneous events with the 'method' set
to 'pt' (per timepoint), 'start' and 'stop' should correspond to the
indices of the first and last *unique* event timepoints for which
statistics are needed. For example, if 'start = 5' and 'stop = 5',
statistics are computed for the 5th unique timepoint in the relational
event sequence, considering all events occurring at unique timepoints
1-4.

## Adjacency matrix

Optionally, a previously computed adjacency matrix can be supplied. Note
that the endogenous statistics will be computed based on this adjacency
matrix. Hence, supplying a previously computed adjacency matrix can
reduce computation time but the user should be absolutely sure the
adjacency matrix is accurate.

## References

Butts, C. T. (2008). A relational event framework for social action.
Sociological Methodology, 38(1), 155–200.
[doi:10.1111/j.1467-9531.2008.00203.x](https://doi.org/10.1111/j.1467-9531.2008.00203.x)
, Stadtfeld, C., & Block, P. (2017). Interactions, actors, and time:
Dynamic network actor models for relational events. Sociological
Science, 4, 318–352.
[doi:10.15195/v4.a14](https://doi.org/10.15195/v4.a14)

## Examples

``` r
library(remstats)

# Tie-oriented model
eff <- ~ inertia():send("extraversion") + otp()
reh_tie <- remify::remify(edgelist = history, model = "tie")
remstats(reh = reh_tie, tie_effects = eff, attr_actors = info)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 5 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: inertia
#>   >> 3: send_extraversion
#>   >> 4: otp
#>   >> 5: inertia:send_extraversion

# Actor-oriented model
seff <- ~ send("extraversion")
reff <- ~ receive("agreeableness") + inertia() + otp()
reh_actor <- remify::remify(edgelist = history, model = "actor")
remstats(
    reh = reh_actor, sender_effects = seff, receiver_effects = reff,
    attr_actors = info
)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: send_extraversion
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 3 statistics
#>   >> Statistics:
#>       >>> 1: receive_agreeableness
#>       >>> 2: inertia
#>       >>> 3: otp
```
