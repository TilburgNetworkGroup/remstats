# Simulated relational event history

A dataset containing a small example of a relational event history. Data
is simulated.

## Usage

``` r
data(history)
```

## Format

A dataframe with 115 rows and 5 variables:

- time:

  time of the event since onset of observation (e.g., in minutes)

- actor1:

  the first actor involved in the event

- actor2:

  the second actor involved in the event

- setting:

  the setting for the event

- weight:

  the intensity of the event (e.g., based on the duration)

## Source

Simulated relational event history for actors in a social network.

## See also

[`info`](https://tilburgnetworkgroup.github.io/remstats/reference/info.md)
for exogenous information on the actors in the social network.

## Examples

``` r
data(history)
```
