# event

Specifies the statistic for an "event" effect in the tie-oriented model.
An "event" effect refers to an exogenous event attribute that affects
the waiting time between events.

## Usage

``` r
event(x, variableName = NULL)
```

## Arguments

- x:

  vector with the event attribute

- variableName:

  optionally, a string indicating the variable name, used for the
  dimnames of the output statistics object

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

The statistic at timepoint *t* is for all dyads in the risk set equal to
the attribute of the event at timepoint *t*.

## See also

[`FEtype`](https://tilburgnetworkgroup.github.io/remstats/reference/FEtype.md)

## Examples

``` r
# \donttest{
  reh_tie <- remify::remify(history, model = "tie")
  data(history, package = "remstats")
  history$work <- ifelse(history$setting == "work", 1, 0)
  effects <- ~ event(x = history$work, variableName = "setting_is_work")
  remstats(reh = reh_tie, tie_effects = effects)
#> Error in FUN(X[[i]], ...): Length of vector 'x' in event() does not match number of events in edgelist
# }
```
