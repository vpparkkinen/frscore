\name{rean_cna}
\alias{rean_cna}
\title{rean_cna}
\usage{
rean_cna(x, attempt = seq(1, 0.7, -0.1), ncsf = deprecated(), output = c("csf", "asf"),
n.init = 1000, ...)
}
\arguments{
\item{x}{A \code{data.frame} or \code{configTable} to be analyzed with \code{\link[cna:cna]{cna()}}. In case of multi-value or fuzzy-set data, the data type must be indicated by \code{type = "mv"} and \code{type = "fs"}, respectively.}

\item{attempt}{Numeric vector that contains the values from which
combinations of consistency and coverage thresholds are formed, to be used
in the analyses.}

\item{ncsf}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Allowed for backward compatibility, due to be dropped in future
versions. Please use \code{n.init} instead.}

\item{output}{Character vector that determines whether csfs, asfs, or mscs are
returned; \code{"csf"} (default) returns csfs, \code{"asf"} asfs,
and \code{"msc"} mscs.}

\item{n.init}{Integer that determines the maximum number of csfs built in
the analyses. See \code{\link[cna:csf]{csf()}}}

\item{...}{Any arguments to be passed to \code{\link[cna:cna]{cna()}} except \code{con}, \code{cov} or
\code{con.msc}. The effect of argument \code{what} is overriden
by \code{output}.}
}
\value{
A list where each element is a data frame containing the results of
a single analysis of the input data set with \code{cna()}, each using a
different combination of consistency and coverage threshold values. These
values are added to the output as extra columns 'cnacon' and 'cnacov'.
}
\description{
Perform a reanalysis series on a data set with \code{\link[cna:cna]{cna()}} using
all combinations of consistency and coverage threshold values in a given
range of values
}
\details{
\code{rean_cna()} performs a reanalysis series of a data set \code{x}, which constitutes the first
computational phase of fit-robustness scoring as introduced in Parkkinen and Baumgartner (2021). The  series consists of \code{\link[cna:cna]{cna()}} calls at all combinatorially possible consistency and coverage settings drawn from the vector \code{attempt}. If the \code{output} argument is set to its default value \code{"csf"}, \code{rean_cna()} returns complex solutions formulas (csf), in case of \code{"asf"} only atomic solution formulas ("asf") are built, which is faster. The argument \code{n.init} allows for controlling the number of csf to be built, if \code{output = "csf"}.
}
\references{
V.P. Parkkinen and M. Baumgartner (2021), \dQuote{Robustness and Model Selection in Configurational Causal Modeling,} \emph{Sociological Methods and Research}, doi:10.1177/0049124120986200.

}
\seealso{\code{\link[frscore:frscore]{frscore()}}, \code{\link[cna:cna]{cna()}}}

\examples{
# Crisp-set data
sols1 <- rean_cna(d.error, attempt = seq(1, 0.8, -0.1))
sols1 <- do.call(rbind, sols1)
sols1

# Multi-value data
sols2 <- rean_cna(d.pban, type = "mv", attempt = seq(0.9, 0.7, -0.1),
                  ordering = list("T", "PB"), strict = TRUE)
sols2 <- do.call(rbind, sols2)
sols2

# Fuzzy-set data
\donttest{sols3 <- rean_cna(d.jobsecurity, type = "fs", attempt = seq(0.9, 0.7, -0.1),
                  ordering = list("JSR"), strict = TRUE) # execution takes a couple of seconds
sols3 <- do.call(rbind, sols2)
sols3}
}
