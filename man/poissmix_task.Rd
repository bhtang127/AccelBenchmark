% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissmix_task.R
\name{poissmix_task}
\alias{poissmix_task}
\title{Poisson mixture problem}
\usage{
poissmix_task()
}
\value{
A list containing all components needed for benchmarking the Poisson mixture problem using Hasselblad data.
 \item{initfn}{Parameter random initializing function}
 \item{fixptfn}{Updating function for the fixed point iteration problem}
 \item{objfn}{Function calculating the objective value for current parameter}
 \item{...}{Other arguments required in functions above}
}
\description{
Poisson mixture parameter estimation problem with Hasselblad data.
}
\examples{
\dontrun{
set.seed(54321)
benchmark(
  poissmix_task,
  algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
  ntimes = 200
)
}

}
\references{
Hasselblad V (1966). Estimation of parameters for a mixture of normal distributions. Technometrics, 8(3): 431–444.
}
