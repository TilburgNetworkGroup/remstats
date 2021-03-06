# `remstats`: Compute statistics for relational event history data. 

Relational event modeling approaches enable researchers to study the exogenous and endogenous predictors of how a time-ordered sequence of relational events - a so-called *relational event history* - evolves over time. Relational event models can be distinguished in so-called *tie-oriented models*, in which the probability of a dyad to interact next is modeled in one step (e.g., see Butts, 2008), and *actor-oriented models*, in which first the probability of a sender to start an interaction is modeled and subsequently the probability of the choice of a receiver of the interaction by the active sender (e.g., see Stadtfeld & Block, 2017). The `R`-package `remstats` computes a variety of statistics for both type of models. 

The `R`-package `remstats` is part of a bundle of `R`-packages developed by researchers from Tilburg University with the aim to assist applied researchers in the application of relational event modeling. For the preparation of the relational event history, `remstats` calls `remify` internally. Estimation of the model can be performed with `remstimate` (when finished). Alternatively (for now), the function `tomstats()` in the `remstats` packages provides the user with every object necessary to perform estimation of a tie-oriented model with `relevent::rem()`.

A table with `remstats` equivalents for statistics in `relevent::rem.dyad()` can be found through [this link](relations.md). 

The following provides a brief introduction in computing statistics for relational event history data with `remstats`. Firstly, we describe the procedure for the tie-oriented model (use the main function `tomstats()`) and secondly for the actor-oriented model (use the main function `aomstats()`). 

## Getting started 

### Installation 
The required packages can be installed from Github. To install the packages, run:
```r
install.packages("devtools")
devtools::install_github("TilburgNetworkGroup/remify")
devtools::install_github("TilburgNetworkGroup/remstats")
```

Load the packages with:
```r
library(remify)
library(remstats)
```

### Data 
Relational event history data describes a time-ordered series of interactions between actors in a network. Such interactions are referred to as relational events. A relational event minimally contains information on the time of the event and the actors that are involved in the event. 

As an illustration, we use the `history` data object in the `remstats` package. This data object is a randomly generated relational event history. A description of the simulated data can be accessed with:

```r 
?history
```  

Here, we read that `history` is a small simulated relational event history with 115 events. Besides information on the time and actors, for each event there is also information on the setting and an event weight. We can view the first six events with:

```r
head(history)
```

time  | actor1    | actor2    | setting   | weight     
---   | ---       | ---       | ---       | ---
238   | 105       | 113       | work      | 1.33
317   | 105       | 108       | work      | 1.64
345   | 115       | 112       | work      | 1.82
627   | 101       | 115       | social    | 1.25
832   | 113       | 107       | social    | 1.67
842   | 105       | 109       | work      | 2.30

Besides the relational event history itself, relational event modeling often requires a second data object with exogenous information on the actors in the network. Information on the actors in the simulated data example in `remstats` is stored in the `info` object. A description of the `info` data can be accessed with:

```r 
?info
```  

Here, we read that the `info` object stores for the 10 actors in the network information on their age, sex, extraversion and agreeableness score. Moreover, extraversion and agreeableness is measured multiple times during the observation period. The time variable tells us when the values change. We can view the attribute information for the first two actors with:

```r
head(info)
```

id  | time  | age   | sex   | extraversion  | agreeableness 
--- |---    | ---   | ---   | ---           | ---
101 | 0     | 0     | 0     | -0.40         | -0.14
101 | 9432  | 0     | 0     | -0.32         | -0.26
101 | 18864 | 0     | 0     | -0.53         | -0.45
102 | 0     | 0     | 0     | -0.13         | -0.65
102 | 9432  | 0     | 0     | -0.43         | -0.44
102 | 18864 | 0     | 0     | -0.13         | -0.43

### The riskset 
The riskset is an important part of relational event modeling. It contains all events that can potentially be observed in the relational event history. Statistics are computed for every event in the riskset for every timepoint. The riskset is prepared by `remify::reh()`, called internally by both `remstats::tomstats()` and `remstats::aomstats()`. 

If nothing is specified by the user, `remify::reh()` assumes that all actors that can potentially interact are observed in the edgelist and creates a riskset with every directed pair of observed actors, for which statistics are then computed. Alternatively, the user can tailor the riskset to its application:

* If not every actor that can potentially interact is observed in the edgelist, the user should supply every actor that can potentially interact to the `actors` argument of `tomstats()` or `aomstats()`. 
* If the relational events in the edgelist and in the riskset are undirected (i.e., no distinction is made between i --> j and j --> i), the user should set `directed = FALSE` in `tomstats()` or `aomstats()`.  
* If the user wants to distinguish between events of different types in the riskset, the user should include a column named `type` with the event types to the edgelist. For example, we could view the settings of the events in `history` as different event types. If we want to distinguish between interaction in a work-related or leisure setting, we rename the fourth column: `names(history)[4] <- "type"`.
* If the user wants to distinguish between events of different types in the riskset but not every type that can potentially occur is observed in the edgelist, the user should supply every type that can potentially occur to the `types` argument of `tomstats()` or `aomstats()`. 
* For other non-standard riskset situations, the user can use the `omit_dyad` argument (see `vignette("remify::reh")`). 

### Compute statistics for the tie-oriented model
The `tomstats()` function in the `remstats`-package computes statistics for modeling relational event history data with a tie-oriented model. The statistics that are requested need to be supplied to the `effects` argument of the function, specified in an object of class `formula`. This specification should be in the form `~ terms`.

An overview of the statistics that can be computed with `tomstats()` is available in the "details" section of the `tomstats()` function documentation (access with `?tomstats`). 

In this illustration, we start with requesting only one statistic: the inertia statistic. Most statistics can be tailored to the user's needs. For example, lets view the description for the `inertia` statistic:
```r
?inertia
```
Here, we can read that the inertia statistic computes for every timepoint *t* for every pair of actors *(i,j)* in the riskset the number of past events. With the `scaling` argument, one of the methods for scaling the statistic can be chosen. With the `consider_type` argument, the user can request to count events between the actor pair *(i,j)* of different types separately. 

In this illustration, we will standardize the inertia statistic. To request this statistic, we define the formula as follows:
```r
effects <- ~ inertia(scaling = "std")
```

Now, we have everything we need to compute our first statistic:
```r
out <- tomstats(effects, edgelist = history)
```

The `tomstats()` function outputs a list with multiple objects. We can view the names of these objects with:
```r
names(out)
```
Here, we can see that `tomstats()` outputs an object named `statistics`, `edgelist`, `riskset`, `evls` and `adjmat`. 

The `statistics` object is a 3-dimensional array. On the rows of this array are the timepoints, the columns refer to the potential events in the riskset and the slices refer to the different statistics:
```r
dim(out$statistics)
```
Our statistics object has 115 rows, corresponding to the 115 events in the edgelist. It has 90 columns, corresponding to the 90 events in the riskset. The statistics object has two slices, that is because the baseline statistics is automatically computed when the timing of the events in the relational event history is exact (unless removed by specifying `-1` in the `effects` formula) and saved in the first slice. The `tomstats()` procedure assumes that the timing of the events in the relational event history is exact and the full likelihood is used in the estimation, unless the argument `ordinal` is set `TRUE`. 

We can view the names of the statistics that are in the statistics object with:
```r
dimnames(out$statistics)
```
Here, we see that, indeed, a baseline and inertia statistic are computed. 

Since we did not request anything special for the riskset, it consists of every directed pair of actors observed in the edgelist, which is 10*9 = 90 pairs. These pairs are saved in the `riskset` object. We can ask for the first few lines of this riskset:
```r
head(out$riskset)
```
actor1  | actor2| type  | id
---     | ---   | ---   | ---
101     | 103   | 0     | 1
101     | 104   | 0     | 2
101     | 105   | 0     | 3
101     | 107   | 0     | 4
101     | 109   | 0     | 5
101     | 111   | 0     | 6

Here, we see that the first event in the riskset is the event were actor 101 sends an interaction directed towards actor 103. The type column contains the different event types, since we did not specify event types these are all equal to 0 here. Finally, the id column refers to the column in the statistic object that contains the statistic(s) for this specific dyad. The first column in the statistic object refers to this first event in the riskset, the second column in the statistic object to the second event in the riskset, and so forth. 

The outputted `edgelist` object by `tomstats()` is mainly a control object. It shows the information used by `tomstats()` to create the riskset compute the statistics and should be largely the same as the inputted `edgelist` object. 
```r
head(out$edgelist)
```
time  | actor1    | actor2    | type    | weight
---   | ---       | ---       | ---     | ---
238   | 105       | 113       | 0       | 1.33
317   | 105       | 108       | 0       | 1.64
345   | 115       | 112       | 0       | 1.82
627   | 101       | 115       | 0       | 1.25
832   | 113       | 107       | 0       | 1.67
842   | 105       | 109       | 0       | 2.30

Again, because we did not request the event type to be considered as a dependent variable, the type column here only contains zeros. 

The adjacency matrix object is a helper function that is used internally by `tomstats` but, once obtained, could be supplied in a next run to save computation time. It contains per timepoint (on the rows) per dyad (on the columns) the number or weight of the past events. The weight is affected by the weight of the events in the edgelist and the `memory` and `memoryValue` arguments. 

Finally, the outputted `evls` object is a transformation of the edgelist into a form such that it can be used by `rem()` function of the `relevent` R-package to estimate a relational event model:
```r
library(relevent)
fit <- rem(out$evls, out$statistics, timing = "interval", estimator = "MLE")
summary(fit)
```

Inertia is an example of an *endogenous* statistic: it is a function of the relational event history itself. Next, we are going to add a request for an *exogenous* statistic. For this we need the exogenous information on the actors in the `info` object. 

As an illustration, we are going to request the statistic for an effect of extraversion on sending events, i.e., a send effect. The description of a send effect is accessed with 
```r
?send
```
Here, we read that we need to supply the variable for which we want to specify a sender effect and that this variable should correspond to a column in the attributes object that we supply. Thus, we specify a send effect of extraversion with `send("extraversion", attributes = info)`. Here, we specify the attributes object within the `send()` function. Alternatively, it can be supplied to `tomstats()`. 

Statistics in the `effects` formula should be separated with the `+`. Finally, we add an interaction between the `inertia()` statistic and the `send()` statistic. This can be done by using the `*` or `:` operator:
```r
effects <- ~ inertia(scaling = "std") + send("extraversion", info) + 
    inertia(scaling = "std"):send("extraversion", info) 
out <- tomstats(effects, edgelist = history)
```

### Compute statistics for the actor-oriented model
The procedure to compute statistics for the actor-oriented model is largely similar to what is written above, except that statistics have to be specified separately for the first modeling step (which we to as the *rate model*) and the second modeling step (which we refer to as the *choice model*). The statistics requested for these two modeling steps need to be supplied to two different effects arguments, namely `rateEffects` and `choiceEffects`, respectively.

An overview of the statistics that can be computed in the rate model and the choice model with `aomstats()` is available in the "details" section of the `aomstats()` function documentation (access with `?aomstats`). 

In this illustration, we start with requesting only one statistic for the rate model: the *outdegreeSender* statistic. First, lets view the description for the `outdegreeSender` statistic:
```r
?outdegreeSender
```
Here, we can read that, in the rate model, the outdegreeSender statistic computes for every timepoint *t* for every actors *i* the number of outgoing past events. With the `scaling` argument, one of the methods for scaling the statistic can be chosen. With the `consider_type` argument, the user can request to count events of different types separately. 

To compute the outdegreeSender statistic for the rate model we supply it to the `rateEffects` argument of `aomstats()`:
```r 
effects <- ~ outdegreeSender()
out <- aomstats(rateEffects = effects, edgelist = history)
```

The outputted list of objects is largely similar to the list outputted by `tomstats()`:
```r
names(out)
```

A difference between the two output objects is that the statistics object is now a list with two elements: `rate` and `choice`. Since we did not request any statistics for the `choice` step here, this object is empty. The `rate` object contains the statistic array with the `baseline` statistic (again, automatically computed unless `ordinal = TRUE`), and the requested `outdegreeSender` statistic. 
```r
dimnames(out$statistics$rate)
```

The dimension of `out$statistics$rate` is 115 x 10 x 2. On the rows we have the timepoints, the columns refer to the actors that can be senders and the slices refer to the different statistics. 

Lets extend our model and also request a statistic for the choice step:
```r
rateEffects <- ~ outdegreeSender()
choiceEffects <- ~ inertia()
out <- aomstats(rateEffects = rateEffects, choiceEffects = choiceEffects, edgelist = history)
```

We can access the statistic computed for the choice step with `out$statistics$choice`. In this step, the baseline statistic is not automatically computed (and not defined). Hence, the dimensions of the statistics object for the choice step are 115 x 10 x 1. On the rows we have again the timepoints, on the columns now the receivers and on the slices the statistics. 

Note that the computed values of the statistic in the choice step are equal to the values for this receiver, given the current sender. For example, lets review the first six lines:
```r
head(out$statistics$choice[,,"inertia"])
```

101 | 103 | 104 | 105 | 107 | 109 | 111 | 112 | 113 | 115
--- | --- | --- | --- | --- | --- | --- | --- | --- | ---
0 | 0 | 0 | 0 | 0 | 0.00 | 0 | 0 | 0.00 | 0 
0 | 0 | 0 | 0 | 0 | 0.00 | 0 | 0 | 1.33 | 0  
0 | 0 | 0 | 0 | 0 | 0.00 | 0 | 0 | 0.00 | 0
0 | 0 | 0 | 0 | 0 | 0.00 | 0 | 0 | 0.00 | 0   
0 | 0 | 0 | 0 | 0 | 0.00 | 0 | 0 | 0.00 | 0 
0 | 0 | 0 | 0 | 0 | 1.64 | 0 | 0 | 1.33 | 0 

At the first timepoint, the inertia statistic for all receivers given the current sender (actor 105) is zero because no prior events have occurred. At the second timepoint, the sender is again actor 105. Now the inertia statistic is equal to the weight of the first event for the receiver of that event (actor 113). At the third timepoint, the inertia statistic is again zero for all receivers because now the sending actor is 115, who did not send any prior events. 



