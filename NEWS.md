# remstats 2.0.0

* Added a `NEWS.md` file to track changes to the package.

## New features 
* The function `aomstats()` is added to compute statistics for the actor-oriented model. Effects for the rate-step and the choice-step of this model have to be specified separately, see the function's documentation or the README.md. 
* The recency statistics have been extended and include the following:
    + recencyContinue: refers to the time that has past since dyad *(i,j)* last interacted.
    + recencyReceiveReceiver: refers to the time that has past since receiver *j* last received an event.
    + recencyReceiveSender: refers to the time that has past since receiver *j* last received an event.
    + recencySendReceiver: refers to the time that has past since receiver *j* last received an event.
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