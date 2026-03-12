# Printing Relational Event Network Statistics

Print a
[`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
object in a user-friendly format.

## Usage

``` r
# S3 method for class 'remstats'
print(x, ...)
```

## Arguments

- x:

  object of class
  [`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md).

- ...:

  further arguments passed to or from other methods.

## Value

The function prints formatted information about the remstats object to
the console, presenting details about the relational event network
statistics in a user-friendly format.

## Examples

``` r
rehObject <- remify::remify(edgelist = history, model = "tie")
remstatsObject <- remstats::remstats(reh = rehObject, tie_effects = ~ remstats::inertia())
print(remstatsObject)
#> Relational Event Network Statistics
#> > Model: tie-oriented
#> > Computation method: per time point
#> > Dimensions: 115 time points x 90 dyads x 2 statistics
#> > Statistics:
#>   >> 1: baseline
#>   >> 2: inertia

rehObject <- remify::remify(edgelist = history, model = "actor")
remstatsObject <- remstats::remstats(reh = rehObject, receiver_effects = ~ inertia())
print(remstatsObject)
#> Relational Event Network Statistics
#> > Model: actor-oriented
#> > Computation method: per time point
#> > Sender model: empty
#> > Receiver model:
#>   >> Dimensions: 115 events x 10 actors x 1 statistics
#>   >> Statistics:
#>       >>> 1: inertia
```
