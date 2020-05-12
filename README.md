# remstats

An R-package that can be used to compute statistics for relational event history data. Statistics are computed with the `remstats()` function. Output from the `remstats()` function can be used to fit a relational event model with the `rem` function from the `relevent` R-package (Butts, 2008). 

## Installation
The package can be installed from Github:
``` r
# Install the package
install.packages("devtools")
devtools::install_github("TilburgNetworkGroup/remstats")

# Load the package
library(remstats)
```

## Getting started
For this application, we use the example data set `edgelistD` that is provided with the package. The data set `edgelistD` is a simulated relational event history between 26 actors (actor IDs are denoted by the 26 letters of the alphabet). Events in this simulated relational event history are dyadic and directed. 

```r
data(edgelistD)
```

We may distinguish between endogenous and exogenous statistics. Endogenous statistics refer to statistics that are a function of past events. For example, the endogenous statistic `inertia` captures the tendency of actors to repeatedly send events to the same receivers by counting the number of past events from sender $i$ to receiver $j$. 

We can compute an inertia statistic with the following lines of codes:

```r
out <- remstats(edgelist = edgelistD, effects = "inertia")
str(out$statistics)
```

The `statistics` object in `out` now contains the inertia statistic. 

Exogenous statistics are not a function of past events but allow to include exogenous covariate information. For example, exogenous statistics may encode covariate information for the sender, receiver, the dyad or the environment. The example data set `covar` contains simulated information on two covariate variables (`x1` and `x2`) for the 26 actors together with the time points at which a covariate value changes for a respective actor. 

```r
data(covar)
```

If we request an exogenous effect to be computed from `remstats()` we have to include the covariate information to the `covariates` argument. The covariates argument expects a list with per exogenous effect the respective covariate information in matrix format. In this matrix, the first column should refer to the actor IDs, the second column to the time points at which a covariate value changes (set all to zero for time-invariant covariate information) and in the third column the covariate information. 

For example, suppose we want to request a statistic for the effect of `x1` on sending events. First, we prepare the matrix for this effect:

```r
sender_effect <- covar[,c(1, 2, 3)]
```

Next, we prepare the covariates argument. Note that the names of the elements in the list should correspond to the effects requested. In this case, we want to request `sender_effect` so we name our list element also `sender_effect`:
```r
covariates <- list(sender_effect = sender_effect)
```

Now, we can compute the statistic with `remstats()`:
```r
out <- remstats(edgelist = edgelistD, effects = "sender_effect", 
    covariates = covariates)
str(out$statistics)
```

We may want to compute more than one `sender_effect`. In this case, we can add additional columns with covariate information to our `sender_effect` matrix in the `covariates` argument:

```r
sender_effect <- covar
covariates <- list(sender_effect = sender_effect)
out <- remstats(edgelist = edgelistD, effects = "sender_effect", 
    covariates = covariates)
str(out$statistics)
```

Interaction between two statistics can also be included in the following manner. Suppose we want to include an interaction between the first sender_effect and inertia:
```r
out <- remstats(edgelist = edgelistD, effects = "sender_effect", "inertia", 
    "sender_effect1*inertia", covariates = covariates)
str(out$statistics)
```

The output from `remstats()` may be used to fit a REM model with `relevent::rem()`. For example:
```r
out <- remstats(edgelist = edgelistD, effects = c("baseline", "inertia", 
    "sender_effect"), covariates = covariates)
evls <- out$evls
stats <- out$stats

library(relevent)
fit <- rem(eventlist = evls, statslist = stats, timing = "interval", 
    estimator = "MLE")
summary(fit)
```

While this application concerned a relational event history with directed relational events, the `remstats()` function may also be used for undirected events or events with types. An example data set for a relational event history with undirected events is available through `data(edgelistU)`, for a relational event history with directed events with types through `data(edgelistDT)`, and for a relational event history with undirected events with types through `data(edgelistUT)`. Note that the user needs to indicate to `remstats()` whether events in the history are directed or undirected by setting `directed = TRUE` or `directed = FALSE` respectively and whether events in the history are without or with types by setting `type = FALSE` or `type = TRUE` respectively. Statistics may not be defined for every type of relational event history; restrictions on types of events for which statistics are defined can be find below. 

Statistics
------------
The following provides a brief overview of the statistics currently in the remstats package. More detailed information will follow.

name in `effects`       | short description | additional arguments | defined for
----------------------- | ----------------- | -------------------- | -----------
`baseline`              | The statistic for dyad $(i,j)$ at time $t$ is equal to 1.
`sender_effect`         | The statistic for dyad $(i,j)$ at time $t$ is equal to the covariate value of actor $i$ at time $t$. | `covariates` | `directed = TRUE`
`receiver_effect`       | The statistic for dyad $(i,j)$ at time $t$ is equal to the covariate value of actor $j$ at time $t$. | `covariates` | `directed = TRUE`
`same`                  | The statistic for dyad $(i,j)$ at time $t$ is set to 1 if actor $i$ and $j$ have the same covariate value at time $t$ and set to zero otherwise. | `covariates`
`difference`            | The statistic for dyad $(i,j)$ at time $t$ is equal to the absolute difference between the covariate values of actor $i$ and $j$ at time $t$.| `covariates`
`mean`                  | The statistic for dyad $(i,j)$ at time $t$ is equal to the mean of the covariate values of actor $i$ and $j$ at time $t$. | `covariates`
`min`                   | The statistic for dyad $(i,j)$ at time $t$ is equal to the minimum of the covariate values of actor $i$ and $j$ at time $t$. | `covariates`
`max`                   | The statistic for dyad $(i,j)$ at time $t$ is equal to the maximum of the covariate values of actor $i$ and $j$ at time $t$. | `covariates`
`both_equal_to`         | The statistic for dyad $(i,j)$ at time $t$ is set to 1 if the covariate values of actors $i$ and $j$ are both equal to some value $\theta$ at time $t$ and set to zero otherwise. | `covariates`, `equal_val`
`event_effect`          | The statistic for all dyads at time $t$ is equal to some value that denotes a characteristic for the event at time $t$. | `event_effect`
`type_effect`           | Creates dummy variables for event types. The statistic for dyad with type $(i,j,c)$ at time $t$ is set to 1 if $c$ is equal to $c_i, i = \{1, 2, ... C\}$ where $C$ refers to the number of unique event types and set to zero otherwise. | | `type = TRUE`
`inertia`               | The statistic for dyad $(i,j)$ at time $t$ is equal to the number of past $(i,j)$ events. 
`inertia_weighted`      | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of the weights of past $(i,j)$ events. | `weights`
`inertia_type`          | The statistic for dyad with type $(i,j,c)$ at time $t$ is equal to the number of past $(i,j,c)$ events. | | `type = TRUE`
`inertia_type_weighted` | The statistic for dyad with type $(i,j,c)$ at time $t$ is equal to the sum of the weights of past $(i,j,c)$ events. | `weights` | `type = TRUE`
`reciprocity`           | The statistic for dyad $(i,j)$ at time $t$ is equal to the number of past $(j,i)$ events. | | `directed = TRUE`
`reciprocity_weighted`  | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of the weights of past $(j,i)$ events. | `weights` | `directed = TRUE`
`indegree_sender`       | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $i$ as receiver. | | `directed = TRUE`
`indegree_receiver`     | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $j$ as receiver. | | `directed = TRUE`
`outdegree_sender`      | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $i$ as sender. | | `directed = TRUE`
`outdegree_receiver`    | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $j$ as sender. | | `directed = TRUE`
`totaldegree_sender`    | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $i$ as sender or receiver. | | `directed = TRUE`
`totaldegree_receiver`  | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum of past events with actor $j$ as sender or receiver. | | `directed = TRUE`
`rrank_send`            | The statistic for dyad $(i,j)$ at time $t$ is equal to the inverse rank of actor $j$ among the actors to which actor $i$ has most recently send past events. | | `directed = TRUE`
`rrank_receive`         | The statistic for dyad $(i,j)$ at time $t$ is equal to the inverse rank of actor $j$ among the actors from which actor $i$ has most recently received past events | | `directed = TRUE`
`OTP`                   | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past $(i,h)$ and $(h,j)$ events. | | `directed = TRUE`
`ITP`                   | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past $(j,h)$ and $(h,i)$ events. | | `directed = TRUE`
`OSP`                   | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past $(i,h)$ and $(j,h)$ events. | | `directed = TRUE`
`ISP`                    | The statistic for dyad $(i,j)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past $(h,i)$ and $(h,j)$ events. | | `directed = TRUE`
`shared_partner`        | The statistic for undirected dyad $(i,j)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past undirected $(i,h)$ and $(j,h)$ events. | | `directed = FALSE`
`unique_sp`             | The statistic for undirected dyad $(i,j)$ at time $t$ is equal to the number of actors $h$ with at least one past undirected $(i,h)$ and $(j,h)$ event. | | `directed = FALSE`
`shared_partners_type`  | The statistic for undirected dyad with type $(i,j,c)$ at time $t$ is equal to the sum over all actors $h$ of the minimum number of past undirected $(i,h,c)$ and $(j,h,c)$ events. | | `directed = FALSE` and `type = TRUE`
`unique_sp_type`        | The statistic for undirected dyad with type $(i,j,c)$ at time $t$ is equal to the number of actors $h$ with at least one past undirected $(i,h,c)$ and $(j,h,c)$ event.  | | `directed = FALSE` and `type = TRUE`
`PSAB-BA`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for the dyads $(j,i)$ and set to zero for all other dyads. | | `directed = TRUE`
`PSAB-BY`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for all dyads $(j,h)$ where $h$ is not $i$ and set to zero for all other dyads. | | `directed = TRUE`
`PSAB-XA`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for all dyads $(h,i)$ where $h$ is not $j$ and set to zero for all other dyads. | | `directed = TRUE`
`PSAB-XB`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for all dyads $(h,j)$ where $h$ is not $i$ and set to zero for all other dyads. | | `directed = TRUE`
`PSAB-XY`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for all dyads $(h,k)$ where $h$ and $k$ are not $i$ or $j$ and set to zero for all other dyads. | | `directed = TRUE`
`PSAB-AY`               | After a past event that was send from actor $i$ to actor $j$, the statistic is set to 1 for all dyads $(i,h)$ where $h$ is not $j$ and set to zero for all other dyads. | | `directed = TRUE`




