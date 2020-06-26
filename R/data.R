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