# Plotting Relational Event Network Statistics Distributions

Generate boxplots for a specified effect in a
[`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md)
object.

## Usage

``` r
# S3 method for class 'tomstats'
boxplot(x, effect, by = "timepoints", subset = NULL, outliers = TRUE, ...)
```

## Arguments

- x:

  An object of class
  [`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md)
  containing relational event network statistics.

- effect:

  A character string specifying the name of the effect in 'x' or an
  integer indicating the index of the effect to be plotted.

- by:

  A string indicating whether the statistic is plotted across
  'timepoints' (default) or 'dyads'.

- subset:

  An optional vector specifying a subset of timepoints or dyads to be
  used for plotting. Per default, a maximum of 20 unique timepoints or
  dyads are plotted.

- outliers:

  A logical value specifying whether to include outliers in the plot.

- ...:

  Additional arguments passed to bxp().

## Value

no return value

## Details

This function produces boxplots to visually represent the distribution
of a specified effect in a relational event network, as captured by a
[`tomstats`](https://tilburgnetworkgroup.github.io/remstats/reference/tomstats.md)
object. The 'effect' parameter allows the user to choose a specific
effect for visualization, either by providing the effect's name or its
index within the 'tomstats' object. The 'by' parameter determines
whether the boxplots are created across different 'timepoints' or
'dyads'. Additionally, an optional 'subset' parameter allows the user to
focus on specific timepoints or dyads. If 'subset' is not specified, a
default maximum of 20 unique timepoints or dyads are plotted. The
'outliers' argument, when set to TRUE, includes the representation of
outliers in the boxplots. If set to FALSE, outliers are omitted from the
visualization.

The boxplots are based on the following summary statistics of the data:
The box in the middle represents the interquartile range (IQR) between
the first (Q1) and third quartile (Q3), and the line inside the box
represents the median. The whiskers extend from the box to the minimum
and maximum values within 1.5 times the IQR below Q1 or above Q3.
Outliers beyond the whiskers are plotted individually.

## Examples

``` r
library(remstats)
# Load data
data(history)
# Prepare data
reh <- remify::remify(edgelist = history[,1:3], model = "tie")
# Compute effects
stats <- remstats(reh, tie_effects = ~ inertia())
# Plot the 'inertia' distribution for 20 timepoints
boxplot(stats, effect = "inertia")

# Plot the 'inertia' distribution for 20 dyads
boxplot(stats, effect = "inertia", by = "dyads")

# Plot the 'inertia' distribution for dyads 2:5
boxplot(stats, effect = "inertia", by = "dyads", subset = 2:5)

```
