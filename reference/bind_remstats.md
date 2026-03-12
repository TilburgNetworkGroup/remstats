# Combine two or more remstats objects

Function to bind any number of remstats objects into one while
duplicated statistics in the combined object are removed based on their
name.

## Usage

``` r
bind_remstats(...)
```

## Arguments

- ...:

  Any number of
  [`remstats`](https://tilburgnetworkgroup.github.io/remstats/reference/remstats.md)
  objects. All the `remstats` objects must have matching dimensions,
  except for the third dimension. Note that duplicated statistics in the
  combined object are removed based on their name.

## Value

`statistics ` array with the combined statistics, where rows refer to
time points, columns refer to potential relational event (i.e.,
potential edges) in the risk set and slices refer to statistics

## Examples

``` r
library(remstats)

# Load the data
data(history)
data(info)

# Prepare the data
reh <- remify::remify(edgelist = history, model = "actor")

# Obtain two different statistics objects
effects1 <- ~ inertia():receive("extraversion") + otp()
stats1 <- remstats(receiver_effects = effects1, reh = reh, attr_actors = info)
effects2 <- ~ reciprocity()
stats2 <- remstats(receiver_effects = effects2, reh = reh, attr_actors = info)

# Bind the two statistics objects
statsC <- bind_remstats(stats1, stats2)
```
