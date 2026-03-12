# send

Specifies the statistic for a "send" effect in the tie-oriented model or
the actor activity rate step of the actor-oriented model. A "send"
effect refers to an exogenous actor attribute that affects actor *i*'s
rate of sending events.

## Usage

``` r
send(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data)
```

## Arguments

- variable:

  string with the name of the column in the `attr_actors` object for
  which the statistic has to be computed.

- attr_actors:

  optionally, an object of class
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) that contains
  the attribute, see 'Details.'

- scaling:

  the method for scaling the statistic. Default is to not scale the
  statistic. Alternatively, standardization of the statistic per time
  point can be requested with "std".

- attr_data:

  Deprecated argument. Please use 'attr_actors' instead.

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The statistic at timepoint *t* is equal to the value of the exogenous
attribute for actor *i* at time *t* for all dyads in the risk set that
have actor *i* as sender. Note that a "send" effect is only defined for
directed relational events.

Construct the \`attr_actors\` object as a data frame where each row
represents the attribute value of actor *i* at timepoint *t*:

- name: The actors' name.

- time: The time when the attribute values change.

- variable: The third column contains the attribute used in the
  specification of the "difference" effect. The column name should
  correspond to the string supplied to the `variable` argument in the
  \`difference()\` function.

Note that it is possible to omit the \`attr_actors\` object in the call
of
[`difference()`](https://tilburgnetworkgroup.github.io/remstats/reference/difference.md)
and, instead, supply it in the call of
[`remstats()`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
for multiple exogenous effects.

## Examples

``` r
data(history)
data(info)

# Tie-oriented model
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ send("extraversion")
remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: send_extraversion

# Actor-oriented model
reh_actor <- remify::remify(history, model = "actor")
remstats(reh = reh_actor, sender_effects = effects, attr_actors = info)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model:
#>   >> Dimensions: 115 time points x 10 actors x 2 statistics
#>   >> Statistics:
#>       >>> 1: baseline
#>       >>> 2: send_extraversion
```
