% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{minimum}
\alias{minimum}
\title{minimum}
\usage{
minimum(variable, attributes = NULL, scaling = c("as.is", "std"))
}
\arguments{
\item{variable}{string with the name of the column in the 
\code{attributes} object for which the statistic has to be computed.}

\item{attributes}{optionally, an object of class 
\code{"\link[base]{data.frame}"} that contains the exogenous attributes (see 
details).}

\item{scaling}{the method for scaling the statistic. Default is to not scale 
the statistic but keep it "as.is". Alternatively, standardization of the 
statistic per time point can be requested with "std".}
}
\description{
Specifies the statistic for a `minimum` effect in the \code{effects} 
argument of \code{\link{tomstats}}.
}
\details{
A minimum effect refers to an exogenous actor attribute that affects dyad 
\emph{(i,j)}'s rate of interacting based on the minimum of their values on 
this attribute. The statistic at timepoint \emph{t} for dyad \emph{(i,j)} is 
equal to the minimum of the values of actor \emph{i} and \emph{j} on the 
attribute at timepoint \emph{t}.   

The \code{attributes} object should be constructed as follows: Each row 
refers to the attribute value of actor \emph{i} at timepoint \emph{t}. An 
`id` column is required that contains the actor id (corresponding to the 
actor id's in the edgelist). A `time` column is required that contains the 
time when attributes change (set to zero if none of the attributes vary over 
time). Subsequent columns contain the attributes that are called in the 
specifications of exogenous statistics. Alternatively, a dataframe with 
attributes for all exogenous effects can be defined in the \code{attributes} 
argument of \code{\link{tomstats}}.
}
\examples{
effects <- ~ minimum("extraversion") 
tomstats(effects, edgelist = history, attributes = info)

}
