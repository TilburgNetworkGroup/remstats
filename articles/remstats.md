# remstats

``` r
library(remstats)
```

## Introduction

Relational event modeling approaches enable researchers to investigate
both exogenous and endogenous factors influencing the evolution of a
time-ordered sequence of relational events - commonly known as a
*relational event history*. These models are categorized into
*tie-oriented models*, where the probability of a dyad interacting next
is modeled in a single step (e.g., see Butts, 2008), and *actor-oriented
models*, which first model the probability of a sender initiating an
interaction and subsequently the probability of the senders’ choice of
receiver (e.g., see Stadtfeld & Block, 2017). The `R` package `remstats`
is designed to compute a variety of statistics for both types of models.

The `remstats` package is part of a bundle of `R`-packages developed by
researchers from Tilburg University intended to aid applied researchers
in the application of relational event modeling. For preparing the
relational event history, `remstats` assumes the prior application of
[`remify::remify()`](https://tilburgnetworkgroup.github.io/remify/reference/remify.html)
(available on CRAN). Model estimation can subsequently be executed using
`remstimate` (available on GitHub at
github.com/TilburgNetworkGroup/remstimate).

The following provides a brief introduction to computing statistics for
relational event history data with remstats. We’ll begin with a quick
workflow example, followed by a detailed description of the procedure
for the tie-oriented model. Finally, we’ll provide an explanation of the
procedure for the actor-oriented model.

## Workflow example

``` r
# Load data
data(history)
data(info)

# Define effects
effects <- ~ 1 + send("extraversion", info) + inertia()

# Prepare event history with the 'remify package'
rehObject <- remify::remify(edgelist = history, model = "tie")

# Compute statistics
statsObject <- remstats(reh = rehObject, tie_effects = effects)

# Estimate model parameters with the 'remstimate' package
# fit <- remstimate::remstimate(reh = rehObject, stats = statsObject,
#   method = "MLE", timing = "interval")
```

## Getting started

### Data

Relational event history data describes a time-ordered series of
interactions between actors in a network. Such interactions are referred
to as *relational events*. A relational event minimally contains
information on the time of the event and the actors that are involved in
the event.

As an illustration, we use the `history` data object in the `remstats`
package. This data object is a randomly generated relational event
history. A description of the simulated data can be accessed with
[`?history`](https://tilburgnetworkgroup.github.io/remstats/reference/history.md).
Here, we read that `history` is a small simulated relational event
history with 115 events. Besides information on the time and actors, for
each event there is also information on the setting and an event weight.
We can view the first six events with:

``` r
head(history)
#>   time actor1 actor2 setting weight
#> 1  238    105    113    work   1.33
#> 2  317    105    109    work   1.64
#> 3  345    115    112    work   1.82
#> 4  627    101    115  social   1.25
#> 5  832    113    107  social   1.67
#> 6  842    105    109    work   2.30
```

We prepare the relational event history for computation of statistics
for the tie-oriented model with the `remify` function from the `remify`
package. Whenever the weight variable is present in the `edgelist`
supplied to `remify`, it assumes that we want to use these to weight the
events in the computation of the statistics. In this example, we don’t
want this, thus we set the weight to one for all events:

``` r
history$weight <- 1
reh <- remify::remify(edgelist = history, model = "tie")
```

Besides the relational event history itself, relational event modeling
often requires a second data object with exogenous information on the
actors in the network. Information on the actors in the simulated data
example in `remstats` is stored in the `info` object. A description of
the `info` data can be accessed with
[`?info`](https://tilburgnetworkgroup.github.io/remstats/reference/info.md).
Here, we read that the `info` object stores for the 10 actors in the
network information on their age, sex, extraversion and agreeableness
score. Moreover, extraversion and agreeableness is measured multiple
times during the observation period. The time variable tells us when the
values change. We can view the attribute information for the first two
actors with:

``` r
head(info)
#>   name  time age sex extraversion agreeableness
#> 1  101     0   0   0        -0.40         -0.14
#> 2  101  9432   0   0        -0.32         -0.26
#> 3  101 18864   0   0        -0.53         -0.45
#> 4  103     0   0   0        -0.13         -0.65
#> 5  103  9432   0   0        -0.43         -0.44
#> 6  103 18864   0   0        -0.13         -0.43
```

### Compute statistics for the tie-oriented model

First, we compute statistics for modeling relational event history data
with a tie-oriented model. The statistics that are requested need to be
supplied to the `tie_effects` argument of
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md),
specified in an object of class `formula`. This specification should be
in the form `~ terms`.

An overview of the statistics that can be computed for the tie-oriented
model is available using the
[`tie_effects()`](https://tilburgnetworkgroup.github.io/remstats/reference/tie_effects.md)
function or its help documentation
[`?tie_effects`](https://tilburgnetworkgroup.github.io/remstats/reference/tie_effects.md):

In this illustration, we start with requesting only one statistic: the
inertia statistic. Most statistics can be tailored to the user’s needs.
For example, lets view the description for the `inertia` statistic using
[`?inertia`](https://tilburgnetworkgroup.github.io/remstats/reference/inertia.md).
Here, we can read that the inertia statistic computes for every
timepoint *t* for every pair of actors *(i,j)* in the riskset the number
of past events. With the `scaling` argument, one of the methods for
scaling the statistic can be chosen. The `consider_type` argument is
relevant when event types are in the dependent variable, which we do not
consider in this example.

In this illustration, we will standardize the inertia statistic. To
request this statistic, we define the formula as follows:s

``` r
effects <- ~ inertia(scaling = "std")
```

Now, we have everything we need to compute our first statistic:

``` r
out <- remstats(tie_effects = effects, reh = reh)
```

The
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
function outputs a 3-dimensional array with statistics for the
tie-oriented model. On the rows of this array are the timepoints, the
columns refer to the potential events in the riskset and the slices
refer to the different statistics:

``` r
dim(out)
#> [1] 115  90   2
```

Our statistics object has 115 rows, corresponding to the 115 time points
in the relational event history. It has 90 columns, corresponding to the
90 events in the riskset. The statistics object has two slices, that is
because the baseline statistics is automatically computed when the
timing of the events in the relational event history is exact (unless
removed by specifying `-1` in the `effects` formula) and saved in the
first slice. The
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
procedure assumes that the timing of the events in the relational event
history is exact and the full likelihood is used in the estimation,
unless the argument `ordinal` in
[`remify::remify()`](https://tilburgnetworkgroup.github.io/remify/reference/remify.html)
is set to `TRUE`.

We can view the names of the statistics that are in the statistics
object with:

``` r
out
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: inertia
```

Here, we see that, indeed, a baseline and inertia statistic are
computed.

Since we did not request anything special for the riskset in
[`remify::remify()`](https://tilburgnetworkgroup.github.io/remify/reference/remify.html),
it consists of every directed pair of actors observed in the relational
event history, which is 10\*9 = 90 pairs. These pairs are saved in the
`riskset` attribute. We can ask for the first few lines of this riskset:

``` r
head(attr(out, "riskset"))
#>   sender receiver id
#> 1    101      103  1
#> 2    101      104  2
#> 3    101      105  3
#> 4    101      107  4
#> 5    101      109  5
#> 6    101      111  6
```

Here, we see that the first event in the riskset is the event were actor
101 sends an interaction directed towards actor 103. The id column
refers to the column in the statistic object that contains the
statistic(s) for this specific dyad. The first column in the statistic
object refers to this first event in the riskset, the second column in
the statistic object to the second event in the riskset, and so forth.

Inertia is an example of an *endogenous* statistic: it is a function of
the relational event history itself. Next, we are going to add a request
for an *exogenous* statistic. For this we need the exogenous information
on the actors in the `info` object.

As an illustration, we are going to request the statistic for an effect
of extraversion on sending events, i.e., a send effect. The description
of a send effect is accessed with
[`?send`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md).
Here, we read that we need to supply the variable for which we want to
specify a sender effect and that this variable should correspond to a
column in the `attr_actors` object that we supply. Thus, we specify a
send effect of extraversion with
`send("extraversion", attr_actors = info)`. Here, we specify the
`attr_actors` object within the
[`send()`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md)
function. Alternatively, it can be supplied to
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md).
This is for example useful if you want to compute a bunch of exogenous
statistics using the same `attr_actors` object.

Statistics in the `effects` formula should be separated with the `+`.
Finally, we add an interaction between the
[`inertia()`](https://tilburgnetworkgroup.github.io/remstats/reference/inertia.md)
statistic and the
[`send()`](https://tilburgnetworkgroup.github.io/remstats/reference/send.md)
statistic. This can be done by using the `*` or `:` operator:

``` r
effects <- ~ inertia(scaling = "std") + send("extraversion", info) + 
    inertia(scaling = "std"):send("extraversion", info) 
out <- remstats(tie_effects = effects, reh = reh)
```

### Compute statistics for the actor-oriented model

The procedure to compute statistics for the actor-oriented model is
largely similar to what is written above, except that statistics have to
be specified separately for the sender activity rate step of the model
and the receiver choice step of the model. The statistics requested for
these two modeling steps need to be supplied to two different effects
arguments, namely `sender_effects` and `receiver_effects`, respectively.

An overview of the statistics that are available for the actor-oriented
model in the two modeling steps can be obtained using the
[`actor_effects()`](https://tilburgnetworkgroup.github.io/remstats/reference/actor_effects.md)
function or its help documentation
[`?actor_effects`](https://tilburgnetworkgroup.github.io/remstats/reference/actor_effects.md).

In this illustration, we start with requesting only one statistic for
the sender activity rate step: the *outdegreeSender* statistic. First,
lets view the description for the `outdegreeSender` statistic using
[`?outdegreeSender`](https://tilburgnetworkgroup.github.io/remstats/reference/outdegreeSender.md).
Here, we can read that, in the sender activity rate step of the
actor-oriented model, the outdegreeSender statistic computes for every
timepoint *t* for every actors *i* the number of outgoing past events.
With the `scaling` argument, one of the methods for scaling the
statistic can be chosen.

First, we prepare the event history for computing statistics for an
actor-oriented model:

``` r
reh <- remify::remify(edgelist = history, model = "actor")
```

To compute the outdegreeSender statistic for the sender activity rate
step we supply it to the `sender_effects` argument of
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md):

``` r
effects <- ~ outdegreeSender()
out <- remstats(sender_effects = effects, reh = reh)
```

The outputted remstats object is now a list with two elements:
`sender_stats` and `receiver_stats`:

``` r
names(out)
#> [1] "sender_stats"   "receiver_stats"
```

Since we did not request any statistics for the receiver choice step
here, the `receiver_stats` object is empty. The `sender_stats` object
contains the statistic array with the `baseline` statistic (again,
automatically computed unless `ordinal = TRUE`), and the requested
`outdegreeSender` statistic:

``` r
out
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: outdegreeSender
```

The dimension of `out$sender_stats` is 115 x 10 x 2. On the rows we have
the timepoints, the columns refer to the actors that can be senders and
the slices refer to the different statistics.

Lets extend our model and also request a statistic for the receiver
choice step:

``` r
sender_effects <- ~ outdegreeSender()
receiver_effects <- ~ inertia()
out <- remstats(sender_effects = sender_effects, receiver_effects = receiver_effects, reh = reh)
```

We can access the statistic computed for the receiver choice step with
`out$receiver_stats`. In this step, the baseline statistic is not
automatically computed (and not defined). Hence, the dimensions of the
statistics object for the receiver choice step are 115 x 10 x 1. On the
rows we have again the timepoints, on the columns now the receivers and
on the slices the statistics.

Note that the computed values of the statistic in the receiver choice
step are equal to the values for this receiver, given the current
sender. For example, lets review the first six lines:

``` r
# Set the column names equal to the receivers
colnames(out$receiver_stats) <- attributes(reh)$dictionary$actors$actorName
# Set the rownames equal to the senders
rownames(out$receiver_stats) <- reh$edgelist$actor1
# View the first six lines
head(out$receiver_stats[,,"inertia"])
#>     101 103 104 105 107 109 111 112 113 115
#> 105   0   0   0   0   0   0   0   0   0   0
#> 105   0   0   0   0   0   0   0   0   1   0
#> 115   0   0   0   0   0   0   0   0   0   0
#> 101   0   0   0   0   0   0   0   0   0   0
#> 113   0   0   0   0   0   0   0   0   0   0
#> 105   0   0   0   0   0   1   0   0   1   0
```

At the first timepoint, the inertia statistic for all receivers given
the current sender (actor 105) is zero because no prior events have
occurred. At the second timepoint, the sender is again actor 105. Now
the inertia statistic is equal to the 1 for the receiver of the first
event (actor 113). At the third timepoint, the inertia statistic is
again zero for all receivers because now the sending actor is 115, who
did not send any prior events.
