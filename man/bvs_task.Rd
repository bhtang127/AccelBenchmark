% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bvs_task.R
\name{bvs_task}
\alias{bvs_task}
\title{Variational Bayes Variable Selection Problem}
\usage{
bvs_task(n = 400, p = 1000, sd = 0.1, rate = 0.1)
}
\arguments{
\item{n}{Number of rows in simulated data}

\item{p}{Number of predictors in simulated data}

\item{sd}{Standard deviation used in random initialize function}

\item{rate}{Only \code{floor(rate * p)} components in \eqn{\beta} will be non-zero}
}
\value{
A list containing all components needed for benchmarking the problem
 \item{initfn}{Parameter random initializing function}
 \item{fixptfn}{Updating function for the fixed point iteration problem}
 \item{objfn}{Function calculating the objective value for current parameter}
 \item{...}{Other arguments required in functions above}
}
\description{
A function that will generate the task list for variational Bayes variable selection problem.
Once ran, it will generate a simulated data that \eqn{logit(p_i) = -1 - Z_{i1} - Z_{i2} + X_i^T \beta},
where Z_i1, Z_i2 are independent standard normal variable. \eqn{\beta} are i.i.d follow Normal(0, 0.25) but some of them are set to be zero
X where drawn independently from Binomial(2, p) where p is uniform over (0.05, 0.5)
}
\examples{
\dontrun{
set.seed(54321)
problem = bvs_task(n=200, p=2000, rate=0.1)
benchmark(
  problem,
  algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
  ntimes = 200
)
}

}
\references{
Carbonetto P, Stephens M, et al. (2012). Scalable variational inference for Bayesian variable selection in regression, and its accuracy in genetic association studies. Bayesian Analysis, 7(1): 73–108.
}
