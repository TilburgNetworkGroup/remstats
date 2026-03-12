# Plotting Relational Event Network Statistics Trajectories

Generate line plots to visualize the trajectories of a specified effect
in the sender model of a
[`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md)
object.

## Usage

``` r
# S3 method for class 'aomstats'
plot(x, effect, subset = NULL, ...)
```

## Arguments

- x:

  An object of class
  [`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md)
  containing relational event network statistics.

- effect:

  A character string specifying the name of the effect in 'x' or an
  integer indicating the index of the effect to be plotted.

- subset:

  An optional vector specifying a subset of actors to be used for
  plotting. By default, a maximum of 5 unique actors are used for
  plotting.

- ...:

  Additional arguments passed to plot().

## Details

This function creates line plots to illustrate the temporal trajectories
of a specified effect in a relational event network, as captured in the
sender model by a
[`aomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/aomstats.md)
object. The 'effect' parameter allows users to choose a specific effect
for visualization, either by providing the effect's name or its index
within the 'aomstats' object. An optional 'subset' parameter enables
users to focus on specific actors. If 'subset' is not specified, a
default maximum of 5 unique actors is plotted. These actors are randomly
selected to represent trajectories across the range of different
endpoints for the effect (excluding zero).

## Examples

``` r
library(remstats)
# Load data
data(history)
# Prepare data
reh <- remify::remify(edgelist = history[,1:3], model = "actor")
# Compute effects
stats <- remstats(reh, sender_effects = ~ outdegreeSender())
# Plot the 'outdegreeSender' trajectories 5 actors
plot(stats, effect = "outdegreeSender")

# Plot the 'outdegreeSender' trajectory for a specific actor
plot(stats, effect = "outdegreeSender", subset = 10)

```
