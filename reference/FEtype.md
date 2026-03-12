# FEtype

Specifies the statistic for fixed effects for event types in the
tie-oriented model.

## Usage

``` r
FEtype()
```

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Details

Fixed effects for event types capture the variation in event rate across
different event types (e.g., see Butts, 2008). The specification of
FEtype results in the creation of C-1 statistics, were C is the number
of different event types in the riskset. Let one of the event types,
e.g. *c = 1*, represent the reference category. Than, for every event
type *c = 2, ..., C*, a statistic is created that at timepoint *t* for
dyad *(i,j,c)* is equal to 1 if *c* is equal to the respective event
type and equal to 0 otherwise (i.e., dummy variables are created). Note
that specifying fixed effects for event types is only available when
event types are modeled in the dependent variable.

## See also

[`event`](https://tilburgnetworkgroup.github.io/remstats/reference/event.md)

## Examples

``` r
history$type <- history$setting
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ FEtype()
remstats(reh = reh_tie, tie_effects = effects)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 180 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: FEtype_work
```
