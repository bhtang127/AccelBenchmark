% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpiter.R
\name{fpiter}
\alias{fpiter}
\title{Fixed point iteration}
\usage{
fpiter(par, fixptfn, objfn, control = list(), ...)
}
\arguments{
\item{par}{Vector for initial parameters}

\item{fixptfn}{Fixed point updating function}

\item{objfn}{Objective function}

\item{control}{A list containing parameters controlling the algorithm}

\item{...}{Other arguments required by \code{fixptfn} and \code{objfn}}
}
\value{
A list of results
 \item{par}{Parameter values, x* that are the fixed-point of fixptfn F such that x*=F(x*) if convergence is successful.}
 \item{value.objfn}{The objective function value at termination.}
 \item{fpevals}{Number of times the fixed-point function \code{fixptfn} was evaluated.}
 \item{objfevals}{Number of times the objective function \code{objfn} was evaluated.}
 \item{iter}{Numbers of iteration used at termination. (for different algorithms, multiple fixed point iteration might be evaluated in one iteration)}
 \item{convergence}{An integer code indicating whether the algorithm converges. 1 for convergence and 0 denote failure.}
 \item{objfn.track}{An array tracking objective function values along the algorithm}
 \item{par.track}{A matrix tracking parameters along the algorithm, where each row is an array of parameters at some iteration. If not tracking paramters, this will be \code{NULL}}
}
\description{
Apply given fixed point function iteratively until convergence
}
\details{
Default values of \code{control} are: \code{tol=1e-7, maxiter=2000, convtype="parameter", par.track=FALSE, conv.spec=NULL}
\describe{
 \item{tol}{A small, positive scalar that determines when iterations should be terminated, see \code{convtype} for details. Default is \code{1e-7}}
 \item{maxiter}{An integer denoting the maximum limit on the number of evaluations of \code{fixptfn}. Default is 2000.}
 \item{convtype}{A string indicating the convergence criteria.
                If it is "parameter", the algorithm will termenate when L2 norm of parameters difference \eqn{x_{new} - x_{old} < tol}.
                If it is "objfn", the algorithm will terminate when the absolute difference of objective function \eqn{|L_{new} - L_{old}| < tol}.
                If it is "user" or \code{conv.spec} is not \code{NULL}. Then the convergence is guided by the user defined function \code{conv.spec}.
                Default is "parameter".}
 \item{par.track}{An bool value indicating whether to track parameters along the algorithm. \code{TRUE} for tracking and \code{FALSE} for not. Default is \code{FALSE}}
 \item{conv.spec}{A function for user specified convergence criteria. When using "parameter" or "objfn" option in \code{convtype}, this should be \code{NULL}.
                 The function should have the form \code{f(old_parameter, new_parameter, old_objective, new_objective, tolerance)} and return 1 if convergent, 0 if not.
                 Defalut is \code{NULL}.}
}
}
\examples{
\dontrun{
set.seed(54321)
prob = lasso_task(lam=1)
fpiter(prob$initfn(), prob$fixptfn, prob$objfn, X=prob$X, y=prob$y)
}

}
