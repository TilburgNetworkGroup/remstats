# Relational Event Network Statistics Summaries

Produce summaries of each statistic from a
[`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
object.

## Usage

``` r
# S3 method for class 'remstats'
summary(object, ...)
```

## Arguments

- object:

  object of class
  [`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md).

- ...:

  additional arguments affecting the summary produced.

## Value

The summaries provide information for each statistic included in the
remstats object, offering insights into the distribution and
characteristics of the data.

## Examples

``` r
rehObject <- remify::remify(edgelist = history, model = "tie")
remstatsObject <- remstats::remstats(reh = rehObject, tie_effects = ~ remstats::inertia())
summary(remstatsObject)
#>         baseline   inertia
#> Min.           1 0.0000000
#> 1st Qu.        1 0.0000000
#> Median         1 0.0000000
#> Mean           1 0.9839246
#> 3rd Qu.        1 1.5700000
#> Max.           1 6.2700000

rehObject <- remify::remify(edgelist = history, model = "actor")
remstatsObject <- remstats::remstats(reh = rehObject, receiver_effects = ~ inertia())
summary(remstatsObject)
#> $sender_stats
#> NULL
#> 
#> $receiver_stats
#>           inertia
#> Min.    0.0000000
#> 1st Qu. 0.0000000
#> Median  0.0000000
#> Mean    0.9155304
#> 3rd Qu. 1.4975000
#> Max.    6.2700000
#> 
```
