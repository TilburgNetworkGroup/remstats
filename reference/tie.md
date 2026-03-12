# tie

Specifies the statistic for a "tie" (or, "dyad") effect.

## Usage

``` r
tie(variable, attr_dyads = NULL, scaling = c("none", "std"), x, variableName)
```

## Arguments

- variable:

  A string specifying the attribute to compute the statistic. If
  `attr_dyads` is a `data.frame`, this refers to the column name in
  `attr_actors`. If `attr_dyads` is a `matrix`, this corresponds to the
  name of the exogenous attribute, used to label the statistic in the
  resulting `remstats` object.

- attr_dyads:

  A `data.frame` or `matrix` containing attribute information for dyads.
  If `attr_dyads` is a `data.frame`, the first two columns should
  represent "actor1" and "actor2" (for directed events, "actor1"
  corresponds to the sender, and "actor2" corresponds to the receiver).
  Additional columns can represent dyads' exogenous attributes. If
  attributes vary over time, include a column named "time". If
  `attr_dyads` is a `matrix`, the rows correspond to "actor1", columns
  to "actor2", and cells contain dyads' exogenous attributes.

- scaling:

  The method for scaling the statistic. The default is no scaling.
  Alternatively, standardization of the statistic per time point can be
  requested with "std".

- x:

  Deprecated argument. Please use 'attr_dyads' instead.

- variableName:

  Deprecated argument. Please use 'variable' instead.

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The "tie" effect or "dyad" effect refers to an exogenous dyad attribute
that influences dyad *(i,j)*'s interaction rate (in tie-oriented models)
or the probability of actor *j* being chosen as a receiver for the event
sent by the active sender *i* (in actor-oriented models). The statistic
represents the value of the exogenous attribute for dyad *(i,j)* in the
`attr_dyads` data.

## Examples

``` r
data(history)
data(both_male_long)
effect <- ~ tie(variable = "both_male", attr_dyads = both_male_long)
reh <- remify::remify(history, model = "tie")
remstats(reh = reh, tie_effects = effect)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: tie_both_male

data(both_male_wide)
effect <- ~ tie(variable = "both_male", attr_dyads = both_male_wide)
reh <- remify::remify(history, model = "tie")
remstats(reh = reh, tie_effects = effect)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: tie_both_male
```
