# Simulated exogenous information on actors in a social network.

A dataset containing exogenous information on the actors in the social
network of a relational event history. Data is simulated.

## Usage

``` r
data(info)
```

## Format

A dataframe with 10 rows and 5 variables:

- id:

  numeric id of the actor

- time:

  numeric value, describes when the value of the covariate changes, if
  it changes

- age:

  dichotomized age of the actor (e.g., 0 = below 25, 1 = 25 or older)

- sex:

  dichotomized sex of the actor (e.g., 0 = male, 1 = female)

- extraversion:

  standardized extraversion score of the actor

- agreeableness:

  standardized agreeableness score of the actor

## Source

Simulated exogenous information on actors in a social network.

## See also

[`history`](https://tilburgnetworkgroup.github.io/remstats/reference/history.md)
for the relational event history.

## Examples

``` r
data(info)
```
