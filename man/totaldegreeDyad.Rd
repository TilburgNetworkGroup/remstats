% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{totaldegreeDyad}
\alias{totaldegreeDyad}
\title{totaldegreeDyad}
\usage{
totaldegreeDyad(scaling = c("none", "prop", "std"), consider_type = TRUE)
}
\arguments{
\item{scaling}{the method for scaling the degree statistic. Default is
to not scale the statistic (scaling = "none"). Alternatively,
scaling of the raw degree counts by two times the number of past events at
time t can be requested with 'prop' or standardization of the raw degree
counts per time point can be requested with 'std'.}

\item{consider_type}{logical, indicates whether to count the degrees
separately for each event type (TRUE, default) or sum degrees across
different event types (FALSE).}
}
\value{
List with all information required by `remstats::remstats()` to compute the statistic.
}
\description{
Specifies the statistic for a 'totaldegreeDyad' effect.
}
\details{
The 'totaldegreeDyad' effect refers to the tendency of pairs of actors
(dyads) to increase their interaction rate as the total degree (number of
interactions) of both actors in the pair goes up. To calculate this effect
for a specific pair (i,j) at a given timepoint (t), we sum the degrees of
the two actors in the dyad (i,j).

Additionally, there is an optional scaling method, which can be chosen using
the 'scaling' method. When the 'prop' scaling method is applied, the degree
count is divided by two times the total number of past events. This scaling
converts the statistic into a fraction, representing the proportion of past
events in which at least one actor in the dyad was involved. For the first
timepoint, where no events have previously occurred, it is assumed that each
actor is equally likely to be involved in an event. In this case, the
statistic is set to 1 divided by the total number of actors (N).

The totaldegreeDyad effect is defined for the tie-oriented model and is
applicable to both directed and undirected events.
}
\examples{
reh_tie <- remify::remify(history, model = "tie")
effects <- ~ totaldegreeDyad()
remstats(reh = reh_tie, tie_effects = effects)

}
