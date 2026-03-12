# userStat

Allows the user to add its own pre-computed statistic to the statistics
object and, optionally, interact this statistic with other statistics in
the formula.

## Usage

``` r
userStat(x, variableName = NULL)
```

## Arguments

- x:

  Matrix with number of rows equal to the number of events and number of
  columns equal to the number of dyads in the network (tie-oriented
  model) or the number of actors in the network (actor-oriented model)

- variableName:

  Optionally, a string with the name of the statistic.

## Value

List with all information required by \`remstats::remstats()\` to
compute the statistic.

## Examples

``` r
# \donttest{
 reh <- remify::remify(history, model = "tie")
 actor101Events <- which(history$actor1 == "101" | history$actor2 == "101")
 actor101_stat <- t(sapply(seq_len(nrow(history)), function(i) {
   rep(i %in% actor101Events, reh$D)
 }))
 
 # Main effects only
 effects <- ~ userStat(x = actor101_stat, variableName = "actor101event")
 remstats(reh = reh, tie_effects = effects)
#> Error in eval(y): object 'actor101_stat' not found
 
 # Model with interaction effects
 interaction_effects <- ~ inertia() *
   userStat(x = actor101_stat, variableName = "actor101event")
 remstats(reh = reh, tie_effects = interaction_effects)
#> Error in eval(y): object 'actor101_stat' not found
# }
```
