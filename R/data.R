#' Simulated relational event history 
#' 
#' A dataset containing a small example of a relational event history. Data is 
#' simulated.
#' 
#' @format A dataframe with 115 rows and 5 variables:
#' \describe{
#'  \item{time}{time of the event since onset of observation (e.g., in minutes)}
#'  \item{actor1}{the first actor involved in the event}
#'  \item{actor2}{the second actor involved in the event}
#'  \item{setting}{the setting for the event}
#'  \item{weight}{the intensity of the event (e.g., based on the duration)}
#' }
#' 
#' @seealso \code{\link{info}} for exogenous information on the actors in 
#' the social network. 
"history"

#' Simulated exogenous information on actors in a social network. 
#' 
#' A dataset containing exogenous information on the actors in the social 
#' network of a relational event history. Data is simulated.
#' 
#' @format A dataframe with 10 rows and 5 variables:
#' \describe{
#'  \item{id}{numeric id of the actor}
#'  \item{time}{numeric value, describes when the value of the covariate 
#'   changes, if it changes}
#'  \item{age}{dichotomized age of the actor (e.g., 0 = below 25, 1 = 25 or 
#'   older)}
#'  \item{sex}{dichotomized sex of the actor (e.g., 0 = male, 1 = female)}
#'  \item{extraversion}{standardized extraversion score of the actor}
#'  \item{agreeableness}{standardized agreeableness score of the actor}
#' }
#' @seealso \code{\link{history}} for the relational event history. 
"info"

#' Exogenous Dyad Attribute Matrix: both_male_wide
#'
#' A matrix representing exogenous attributes of dyads in a social network.
#' The matrix indicates whether a dyad consists of two male actors (sex=0).
#' Rows and columns correspond to actor IDs, and cells contain binary values
#' (1 for male-male dyads, 0 otherwise).
#'
#' @format A square matrix with dimensions equal to the number of unique actors.
#' @usage data(both_male_wide)
#' @seealso \code{\link{tie}} for the function using this data, \code{\link{both_male_long}} for the data in long format, and \code{\link{info}} for an overview of the actor exogenous attributes.
#' @source Simulated exogenous information on actors in a social network.
#' 
#' @examples
#' data(both_male_wide)
#' print(both_male_wide)
#' 
#' @keywords dataset
"both_male_wide"

#' Exogenous Dyad Attribute in Long Format: both_male_long
#'
#' A data frame representing exogenous attributes of dyads in a social network in long format. Each row indicates whether a dyad consists of two male actors (sex=0) in the original matrix `info_both_male_wide`.
#'
#' @format A data frame with the following columns:
#' \describe{
#'  \item{actor1}{Numeric id of the first actor in the dyad.}
#'  \item{actor2}{Numeric id of the second actor in the dyad.}
#'  \item{both_male}{Binary indicator (1 for male-male dyads, 0 otherwise).}
#' }
#' @usage data(both_male_long)
#' @seealso \code{\link{tie}} for the function using this data, \code{\link{both_male_wide}} for the data in wide format, and \code{\link{info}} for an overview of the actor exogenous attributes.
#' @source Simulated exogenous information on actors in a social network.
#'
#' @examples
#' data(both_male_long)
#' head(both_male_long)
#'
#' @keywords dataset
"both_male_long"
