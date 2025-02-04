% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark.R
\name{summary.benchmark}
\alias{summary.benchmark}
\title{Summary Method for \code{benchmark} Class}
\usage{
\method{summary}{benchmark}(object, rate_tol = 0.1, loss_tol = NULL, ci_level = 0.95, ...)
}
\arguments{
\item{object}{benchmark class output by benchmark function}

\item{rate_tol}{A value in (0, 1). All repetitions with loss bigger than \eqn{(1+rate_tol) * minimum_loss} would be considered as a failure.}

\item{loss_tol}{A real value. All repetitions with loss bigger than \code{loss_tol} would be considered as a failure.}

\item{ci_level}{A value in (0, 1) for confidence level of confidence intervals in the result}

\item{...}{Other arguments for summary function.}
}
\value{
A data frame containing useful values illustrating the performance of different algorithms.
}
\description{
Summary Method for \code{benchmark} Class
}
\details{
If \code{loss_tol} is not \code{NULL}, \code{rate_tol} will not be used.
}
\examples{
\dontrun{
set.seed(54321)
out = benchmark("poissmix", c("raw", "squarem", "daarem", "pem", "qn", "nes"),  ntimes=100)
summary(out)
}

}
