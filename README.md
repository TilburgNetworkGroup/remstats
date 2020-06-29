# `remstats`: Compute statistic for relational event history data. 

Butts' (2008) relational event model allows researchers to investigate the exogenous and endogenous predictors of how a time-ordered 
sequence of relational events (a so-called *relational event history*) evolves over time. The R-package `remstats` is developed to assist researchers in applying 
relational event modeling to their own data: it computes a range of exogenous and endogenous statistics for relational event history data. 

The `remstats()` function is the main function of the package. It requires as input an `edgelist` (the relational event history in a `matrix` or `data.frame`) and a `formula` object that specifies the statistics that need to be computed. Additionally, a `covariates` object may contain exogenous information on the actors in the social network that may interact. An introduction on how the `remstats()` function can be used to compute statistics for relational event history data follows below. 

## Getting started 

### Installation 
The package can be installed from Github. To install the package, run:
```r
install.packages("devtools")
devtools::install_github("TilburgNetworkGroup/remstats")
```

Load the package with:
```r
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

Here, we read that the `info` object stores for the 10 actors in the network information on their age, sex, extraversion and agreeableness score. Moreover, extraversion and agreeableness is measured multiple times during the observation period. The time variable tells us when the values change. We can view covariates information for the first two actors with:

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
The riskset is an important part of relational event modeling. It contains all events that can potentially be observed in the relational event history. Statistics are computed for every event in the riskset for every timepoint. Therefore, the riskset is also an important part of `remstats`. If nothing is specified by the user, `remstats()` assumes that all actors that can potentially interact are observed in the edgelist and creates a riskset with every directed pair of observed actors, for which statistics are then computed. Alternatively, the user can use the `directed`, `with_type`, `riskset`, `actors`, and `types` arguments of `remstats()` to tailor the riskset to its application:

* If not every actor that can potentially interact is observed in the edgelist, the user should supply every actor that can potentially interact to the `actors` argument. 
* If the relational events in the edgelist and in the riskset are undirected (i.e., no distinction is made between i --> j and j --> i), the user should set `directed = FALSE`. 
* If the user wants to distinguish between events of different types in the riskset (e.g., setting in the illustrative example), the user should set `with_type = TRUE`.
* If the user wants to distinguish between events of different types but not every type that can potentially occur is observed in the edgelist, the user should supply every type that can potentially occur to the `types` argument. 
* For other non-standard riskset situations, the user should create its own riskset object and supply it to the `riskset` argument. 

### Compute statistics
The `remstats()` function in the `remstats`-package computes statistics for the relational event history data. The statistics that are requested need to be supplied to the `formula` argument of the function, specified in an object of class `formula`. This specification should be in the form `~ terms`.

An overview of the statistics that can be computed with remstats is available in the "details" section of the `remstats` function documentation (access with `?remstats`). 

In this illustration, we start with requesting only one statistic: the inertia statistic. Most statistics can be tailored to the user's needs. For example, lets view the description for the `inertia` statistic:
```r
?inertia
```
Here, we can read that the inertia statistic computes for every timepoint *t* for every pair of actors *(i,j)* in the riskset the number of past events. With the `scaling` argument, one of the methods for scaling the statistic can be chosen. With the `memory_value` argument, the user can indicate the time interval for which past events should be included in the statistic. With the `with_type` argument, the user can request to count events between the actor pair *(i,j)* of different types separately. Finally, with the `event_weights` argument, the user can indicate the weight with which past events should be included in the count. 

In this illustration, we will standardize the inertia statistic and use the information on the event weights in the edgelist to weight the events in the counts. To request this statistic, we define the formula as follows:
```r
formula <- ~ inertia(scaling = "standardize", event_weights = history$weight)
```

Now, we have everything we need to compute our first statistic:
```r
out <- remstats(formula, edgelist = history)
```

The `remstats()` function outputs a list with multiple objects. We can view the names of these objects with:
```r
names(out)
```
Here, we can see that `remstats()` outputs an object named `statistics`, `edgelist`, `riskset` and `evls`. 

The `statistics` object is a 3-dimensional array. On the rows of this array are the timepoints, the columns refer to the potential events in the riskset and the slices refer to the different statistics:
```r
dim(out$statistics)
```
Our statistics object has 115 rows, corresponding to the 115 events in the edgelist. It has 90 columns, corresponding to the 90 events in the riskset. Since we did not request anything special for the riskset, it consists of every directed pair of actors observed in the edgelist, which is 10*9 = 90 pairs. These pairs are saved in the `riskset` object. We can ask for the first few lines of this riskset:
```r
head(out$riskset)
```
sender  | receiver
---     | ---
103     | 101
104     | 101
105     | 101
107     | 101
109     | 101
111     | 101

Here, we see that the first event in the riskset is the event were actor 103 sends an interaction directed towards actor 101. The first column in the statistic object refers to this first event in the riskset, the second column in the statistic object to the second event in the riskset, and so forth. 

Finally, our statistics object has only one slice, since we only requested one statistic. 

The outputted `edgelist` object by `remstats()` is mainly a control object. It shows the information used by `remstats()` to create the riskset compute the statistics and should be the same as the inputted `edgelist` object. It contains an extra column with for every event that is observed its position in the riskset:
```r
head(out$edgelist)
```
time  | actor1    | actor2    | rs_position
---   | ---       | ---       | ---
238   | 105       | 113       | 76
317   | 105       | 108       | 49
345   | 115       | 112       | 72
627   | 101       | 115       | 82
832   | 113       | 107       | 44
842   | 105       | 109       | 49

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
Here, we read that we need to supply the variable for which we want to specify a sender effect and that this variable should correspond to a column in the covariates object that we supply. Thus, we specify a send effect of extraversion with `send("extraversion", info)`.

We further add a `baseline()` statistic to our formula. Statistics in the formula should be separated with the `+`. Finally, we add an interaction between the `inertia()` statistic and the `send()` statistic. This can be done by using the `*` or `:` operator:
```r
formula <- ~ baseline() + inertia(scaling = "standardize", 
    event_weights = history$weight):send("extraversion", info)
out <- remstats(formula, edgelist = history)
```
