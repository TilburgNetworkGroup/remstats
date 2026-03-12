# minimum

Specifies the statistic for a "minimum" effect in the tie-oriented
model. A "minimum" effect refers to an exogenous actor attribute that
affects dyad *(i,j)*'s rate of interacting based on the minimum of the
values of actors *i* and *j* on this attribute.

## Usage

``` r
minimum(variable, attr_actors = NULL, scaling = c("none", "std"), attr_data)
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

The statistic at timepoint *t* for dyad *(i,j)* is equal to the minimum
of the values of actor *i* and *j* on the attribute at timepoint *t*.

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
reh_tie <- remify::remify(history, model = "tie", directed = FALSE)
effects <- ~ minimum("extraversion")
remstats(reh = reh_tie, tie_effects = effects, attr_actors = info)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 45 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: minimum_extraversion
```
