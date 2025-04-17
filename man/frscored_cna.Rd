\name{frscored_cna}
\alias{frscored_cna}
\title{frscored_cna}
\usage{
frscored_cna(
  x,
  fit.range = c(1, 0.7),
  granularity = 0.1,
  output = c("csf", "asf", "msc"),
  normalize = c("truemax", "idealmax", "none"),
  verbose = FALSE,
  maxsols = 50,
  test.model = NULL,
  print.all = FALSE,
  comp.method = c("causal_submodel", "is.submodel"),
  n.init = 1000,
  ...
)
}
\arguments{
\item{x}{A \code{data.frame} or \code{configTable} to be analyzed with
\code{\link[cna:cna]{cna()}}. In case of multi-value or fuzzy-set data, the data type must be indicated by \code{type = "mv"} and \code{type = "fs"}, respectively.}

\item{fit.range}{Numeric vector of length 2; determines the maximum and
minimum values of the interval of consistency and coverage thresholds used in the
reanalysis series. Defaults to \code{c(1, 0.7)}.}

\item{granularity}{Numeric scalar; consistency and coverage are varied by
this value in the reanalysis series. Defaults to \code{0.1}.}

\item{output}{String that determines whether csfs, asfs, or mscs are
returned; \code{"csf"} (default) returns csfs, \code{"asf"} asfs,
and \code{"msc"} mscs.}

\item{normalize}{String that determines the method used in
normalizing the scores. \code{"truemax"} (default) normalizes by the highest score among the elements of \code{sols},
such that the highest scoring solution types get score 1. \code{"idealmax"}
normalizes by a theoretical maximum score (see Details). }

\item{verbose}{Logical; if \code{TRUE}, additional
information about causal compatibility relations among the unique solution types found
in \code{sols} is printed. Defaults to \code{FALSE}.}

\item{maxsols}{Integer determining the maximum number of unique solution
types found in the reanalysis series to be included in the scoring (see Details).}

\item{test.model}{String that specifies a single candidate
\code{cna()} solution/model whose fit-robustness score is calculated against
the results of the reanalysis series.}

\item{print.all}{Logical that controls the number of entries printed when
printing the results. If \code{TRUE}, results are printed as when using the defaults of
\code{print.data.frame}. If \code{FALSE}, 20 highest scoring
solutions/models are printed.}

\item{comp.method}{String that determines how the models in \code{sols} are compared
to determine their fr-score. \code{"causal_submodel"} (the default) checks for
causal submodel relations using \code{causal_submodel()},
\code{"is.submodel"} checks for syntactic submodel relations with \code{is.submodel()}}

\item{n.init}{Integer that determines the maximum number of csfs built in
the analyses, see \code{\link[cna:csf]{cna::csf()}}. Only applied when \code{output = "csf"}.}

\item{...}{Any arguments to be passed to \code{\link[cna:cna]{cna()}} except \code{con}, \code{cov} or
\code{con.msc}. The effect of argument \code{what} is overriden
by \code{output}.}
}

\value{
A list whose first element is a data frame that contains the model types
returned from a reanalysis series of the input data, their details
such as consistency and coverage, together with the unadjusted fit-robustness score
of each model type shown in column 'score', and a normalized score in column
'norm.score' in case \code{normalize = "truemax"} or \code{normalize = "idealmax"}. The other elements
contain additional information about the submodel relations among
the unique solution types and about how
the function was called.
}

\description{
Perform a reanalysis series on a data set and calculate the
fit-robustness scores of the resulting solutions/models
}

\details{
\code{frscored_cna()} is a wrapper function that sequentially executes \code{\link[frscore:rean_cna]{rean_cna()}} and \code{\link[frscore:frscore]{frscore()}}, meaning it performs both computational phases of fit-robustness scoring as introduced in Parkkinen and Baumgartner (2021). In the first phase, the function conducts a reanalysis series on the input data \code{x} at all combinatorially possible combinations of fit thresholds that can be generated from the interval given by \code{fit.range} at increments given by \code{granularity} and collects all solutions/models in a set \strong{M}. In the second phase, it calculates the fit-robustness scores of the atomic (asf) and/or complex (csf) solution formulas in \strong{M}.
The argument \code{output} allows for controlling whether csf or only asf are built, the latter normally being  faster but less complete.

The fit-robustness scores can be normalized in two ways. In the default setting \code{normalize = "truemax"}, the score of each \code{sols[i]} is divided by the maximum score obtained by an element of \code{sols}. In case of \code{normalize = "idealmax"}, the score is normalized not by an actually obtained
maximum but by an idealized maximum, which is calculated by assuming that all solutions of equal
complexity in \code{sols} are identical and that for every \code{sols[i]} of a given complexity, all less complex
elements of \code{sols} are its submodels and all more complex elements of \code{sols} are its supermodels.
When normalization is applied, the normalized score is shown in its own column \code{norm.score} in
the results. The raw scores are shown in the column \code{score}.


If the argument \code{verbose} is set to \code{TRUE}, \code{frscored_cna()} also
prints a list indicating for each solution/model how many raw score points it receives from which elements of \strong{M}. The verbose list is ordered with decreasing fit robustness scores.

If the size of the consistency and coverage range scanned in the reanalysis series generating \strong{M} is large or there are many model ambiguities, \strong{M} may contain so many different types of solutions that robustness cannot be calculated for all of them in reasonable time. In that case, the argument \code{maxsols} allows for capping the number of solution types to be included in the scoring (defaults to 50). \code{frscored_cna()} then selects the most frequent solutions in \strong{M} of each complexity level until \code{maxsols} is reached and only scores the thus selected elements of \strong{M}.

If the user is interested in the robustness of one specific candidate model, that model can be given to \code{frscored_cna()} by the argument \code{test.model}. The result for that model will then be printed separately, provided the model is found in the
reanalysis series, if not, the function stops.
}
\seealso{\code{\link[frscore:frscore]{frscore()}}, \code{\link[frscore:rean_cna]{rean_cna()}},
\code{\link[frscore:causal_submodel]{causal_submodel()}}, \code{\link[cna:is.submodel]{cna::is.submodel()}}
}
\references{
P. Emmenegger (2011) \dQuote{Job Security Regulations in Western Democracies:
A Fuzzy Set Analysis.} \emph{European Journal of Political Research} 50(3):336-64.

C. Hartmann and J. Kemmerzell (2010) \dQuote{Understanding Variations
in Party Bans in Africa.} \emph{Democratization} 17(4):642-65.
doi:10.1080/13510347.2010.491189.

V.P. Parkkinen and M. Baumgartner (2021), \dQuote{Robustness and Model Selection in Configurational Causal Modeling,} \emph{Sociological Methods and Research}, doi:10.1177/0049124120986200.

}\examples{
# Robustness analysis from sect. 4 of Parkkinen and Baumgartner (2021)
frscored_cna(d.error, fit.range = c(1, 0.75), granularity = 0.05,
             ordering = list("E"), strict = TRUE)

# Multi-value data from Hartmann and Kemmerzell (2010)
\donttest{frscored_cna(d.pban, type = "mv", fit.range = c(0.9, 0.7), granularity = 0.1,
                  normalize = "none", ordering = list("T", "PB"), strict = TRUE)}

# Fuzzy-set data from Emmenegger (2011)
\donttest{frscored_cna(d.jobsecurity, type = "fs", fit.range = c(0.9, 0.6), granularity = 0.05,
                  ordering = list("JSR"), strict = TRUE)}

# Artificial data
\donttest{dat <- data.frame(
  A = c(1,1,0,0,0,0,1,1),
  B = c(0,1,0,0,0,0,1,1),
  C = c(1,0,1,0,1,0,1,0),
  D = c(1,1,0,0,1,1,0,0),
  E = c(1,1,1,1,0,0,0,0))
frscored_cna(dat)
frscored_cna(dat, output = "asf")
frscored_cna(dat, maxsols = 10)
frscored_cna(dat, test.model = "(b*e+A*E<->D)*(B<->A)")
}
}
