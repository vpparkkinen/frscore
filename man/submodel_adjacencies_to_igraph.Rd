% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_submodel_network.R
\name{submodel_adjacencies_to_igraph}
\alias{submodel_adjacencies_to_igraph}
\title{Generate igraph from submodel adjacencies.}
\usage{
submodel_adjacencies_to_igraph(x)
}
\arguments{
\item{x}{An object of class \code{"frscore"} or \code{"frscored_cna"}.}
}
\value{
An \code{igraph} graph.
}
\description{
Generate igraph from submodel adjacencies.
}
\details{
\code{\link[=frscore]{frscore()}} and \code{\link[=frscored_cna]{frscored_cna()}}
output includes an adjacency matrix of submodel relations among
the scored model types. \code{submodel_adjacencies_to_igraph()} is a convenience
function that extracts the adjacency matrix and produces an \code{igraph} graph
from it.
}
\examples{
r <- frscored_cna(d.error)
sg <- submodel_adjacencies_to_igraph(r)
sg
}
