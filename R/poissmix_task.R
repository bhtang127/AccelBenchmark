# EM algorithm for Poisson mixture update
poissmix_update = function(par,y) {
  # The fixed point mapping giving a single E and M step of the EM algorithm
  pnew <- rep(NA,3)
  i <- 0:(length(y)-1)
  zi <- par[1]*exp(-par[2])*par[2]^i / (par[1]*exp(-par[2])*par[2]^i + (1 - par[1])*exp(-par[3])*par[3]^i)
  pnew[1] <- sum(y*zi)/sum(y)
  pnew[2] <- sum(y*i*zi)/sum(y*zi)
  pnew[3] <- sum(y*i*(1-zi))/sum(y*(1-zi))
  par <- pnew
  return(pnew)
}

# Negative log likelihood for Poisson mixture
poissmix_loss <- function(par,y) {
  # Objective function whose local minimum is a fixed point
  # negative log-likelihood of binary poisson mixture
  i <- 0:(length(y)-1)
  loglik <- y*log(par[1]*exp(-par[2])*par[2]^i/exp(lgamma(i+1)) +
                    (1 - par[1])*exp(-par[3])*par[3]^i/exp(lgamma(i+1)))
  return ( -sum(loglik) )
}

# init function
poissmix_init = function(){
  c(runif(1), runif(2, 0, 4))
}

#' Poisson mixture problem
#'
#' Poisson mixture parameter estimation problem with Hasselblad data.
#'
#' @return A list containing all components needed for benchmarking the Poisson mixture problem using Hasselblad data.
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Hasselblad V (1966). Estimation of parameters for a mixture of normal distributions. Technometrics, 8(3): 431–444.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' benchmark(
#'   poissmix_task,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 200
#' )
#' }
#'
#' @export poissmix_task
poissmix_task = function(){
  list(
    initfn = poissmix_init,
    fixptfn = poissmix_update,
    objfn = poissmix_loss,
    y = AccelBenchmark::Hasselblad$freq
  )
}
