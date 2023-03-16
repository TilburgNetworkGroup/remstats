# remstats 3.0.3
Date: 16-Mar-23

## New features
* Interval memory. 
* Obtain a list with available effects with the functions `tie_effects()` and `actor_effects()`.
* Show progress with `display_progress` argument in `aomstats()` (will be added to `remstats()` and `tomstats()` later.)

## Minor changes 
* Fixed bug in exogenous stats with time-varying attributes in the tie-oriented model.
* Fixed bug in scaling the exogenous stats in the receiver step of the actor-oriented model. 
* Added explanatory warnings and errors. 
* Fixed partial match warning (issue #35). 
* Fixed environment issue attributes object. 
* Renamed "Brandes" memory to "decay".
* Updated computation degree, inertia, reciprocity and triadic statistics in actor-oriented model: without adjacency matrix (issue #39). 

# remstats 3.0.2
Date: 08-Feb-23

## New features 
* psABAB() effect
* psABAY() effect adapted for undirected events
* degreeDiff() effect

# remstats 3.0.1
Date: 05-Jan-23

## New features 
* userStat() effect

# remstats 3.0.0
Date: 22-Dec-21

IMPORTANT: remstats 3.0.0 works with remify >= 2.0.0. Make sure remify is updated. If earlier versions of remify are in use, remstats 3.0.0 will break down. 

## Major changes
* Integrated new version (2.0.0) of remify. 

## Minor changes
* Fixed bug in `tie()` (see issue [#29](https://github.com/TilburgNetworkGroup/remstats/issues/29)).
* Added warning messages when redundant actors are present in input data for the exogenous stats (see issue [#30](https://github.com/TilburgNetworkGroup/remstats/issues/30)).

# remstats 2.0.4
Date: 21-Dec-21

## New features
* Enabled ARMA_64BIT_WORD

# remstats 2.0.3
Date: 16-Dec-21

## New features
* Added `degreeMin` and `degreeMax` statistics for undirected events.
* Added `ccp` statistic for undirected, dyadic events. 
* Option to output only statistics (since outputting all objects may take a lot of time).

## Major changes
* Updated computation ``Brandes memory'' in adjmat to include normalization factor. 

# remstats 2.0.2

## New features
* Added `verbose` argument that, when set to TRUE, outputs a progress update on the statistics computation.
* Added vector with types names to output object of `tomstats()`. 

## Major changes 
* Changed the specification of the variable in the `event()` effect. 

## Minor changes
* Updated computation procedure for triadic and rrank statistics in the 
tie-oriented model (`tomstats`) for greater efficiency and less computation 
time. 
* Changed the way interaction dimnames of the statistics are defined so its more informative. 
* Updated documentation. 

# remstats 2.0.1

## New features
* Added the `remstats()` function, which is a wrapper for `aomstats()` and `tomstats()`. 
* Added vector with actor names to output object. 

## Major changes 
* Changed `aomstats()` function arguments that refer to the requested effects: `sender_effects` (was `rateEffects`) and `receiver_effects` (was `choiceEffects`). 
* Changed names of the `aomstats()` `statistics` List output object to `sender_stats` (was `rate`) and `receiver_stats` (was `choice`). 
* Fixed bug in computation "spUnique" effect (was affected by event weights). 
* Added variableName tie to dimnames statistics object.  
* Fixed bug in computation "event" effect in combination with windowed memory (covariates object was not sliced). 
* Fixed bug in computation "tie" effect (wrong ordering). 

## Minor changes
* Updated package description.
* Updated functions documentation. 

# remstats 2.0.0

* Added a `NEWS.md` file to track changes to the package.

## New features 
* The function `aomstats()` is added to compute statistics for the actor-oriented model. Effects for the rate-step and the choice-step of this model have to be specified separately, see the function's documentation or the README.md. 
* The recency statistics have been extended and include the following:
    + recencyContinue: refers to the time that has past since dyad *(i,j)* last interacted.
    + recencyReceiveReceiver: refers to the time that has past since receiver *j* last received an event.
    + recencyReceiveSender: refers to the time that has past since sender *i* last received an event.
    + recencySendReceiver: refers to the time that has past since receiver *j* last sent an event.
    + recencySendSender: refers to the time that has past since sender *i* last sent an event.
* The option `Brandes` is added to the `memory` functionality. This refers to the exponential decay of the weight of past events, depending on the time that has past since the event occurred. The `memoryValue` argument refers to the halftime parameter (see Brandes et al., 2009). 

## Major changes
* The name of the function `remstats()`, which was the main function in the previous version, is changed to `tomstats()` (because it computes statistics for the tie-oriented model, as opposed to the function `aomstats()`, which computes statistics for the actor-oriented model). 
* `memory` and `memoryValue` are added as arguments of `tomstats()` and `aomstats()` and removed from the separate statistic functions. This is because based on these memory settings, an adjacency matrix is computed. Based on this adjacency matrix, a lot of the endogenous effects are computed. While this increases the efficiency of the computation, memory effects can no longer be specified separately per endogenous variable.
* event weights have to be specified as a separate `weight` column in the `edgelist` that is supplied to `tomstats()` or `aomstats()` instead of to a separate argument in the separate statistic functions.  This is because based on these weights, an adjacency matrix is computed that is used for the computation of the endogenous effects. (see previous point).
* Effects that are requested should be specified in the `effects` argument (`tomstats()`) or `rateEffects` and `choiceEffects` argument (`aomstats()`). Previously, this argument was called `formula`. 
* The actor and event types in the edgelist and riskset output of `tomstats()` and `aomstats()` are transformed back to their original ID's, instead of to the ID's used by `remify::reh()` and internally, so that the user can more easily assess the computed statistics. 
* An intercept is specified in the same manner as in `lm()`. In the tie-oriented model and rate step of the actor-oriented model, an intercept term is assumed by default (unless the `ordinal` argument is set to `TRUE`). Alternatively, it can be explicitly specified by adding 1 to the terms of the effects formula or explicitly removed by adding -1 to the terms. The `baseline()` specification is removed. 
* The exogenous statistic equate() has been removed, something similar can be achieved with tie().
* Fixed effects for the event type can now be specified with `FEtype()` (could previously be done with `baseline(with_type = TRUE)`). 
* The argument `with_type` in the endogenous statistic functions is renamed to `consider_type`. 
* Previously, two or more variables could be specified in one exogenous effect formula. This functionality is removed, only one variable per exogenous effect formula can be specified. 


## Minor changes
* An `attributes` object (previously named `covariates`) can now be supplied to the main functions `tomstats()` or `aomstats()` directly, but can also still be specified in the separate functions for exogenous statistics. 
* To increase efficiency, computation of some of the statistics is based on an adjacency matrix that is either first computed internally or can be supplied by the user. The user won't notice much from this, except that the adjacency matrix (if computed) is also outputted and can be inputted again to decrease computation time if an extra statistic is requested. 