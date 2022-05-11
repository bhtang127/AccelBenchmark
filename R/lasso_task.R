SoftThresh <- function(x, lambda) {
  sign(x)*pmax(abs(x) - lambda, 0.0)
}

expit <- function(x) 1 / (1 + exp(-x))

# LASSO proximal gradient descent step
GDstep <- function(par, X, y, lambda, stplngth) {
  beta.vec <- par
  rr <- y - expit(c(X%*%beta.vec)) ## residuals
  beta.vec <- SoftThresh(beta.vec + stplngth*c(crossprod(X, rr)), lambda=lambda*stplngth)
  return(beta.vec)
}

# LASSO loss
LogisticLoss <- function(par, X, y, lambda) {
  X.beta <- as.vector(X %*% par)
  p.hatm1 <- log1p(exp(X.beta))
  ans <- (-1) * sum(y * X.beta) + sum(p.hatm1) + lambda * sum(abs(par))
  return(ans)
}


# init function
lasso_init = function(p){
  runif(p, -1, 1)
}

#' LASSO logistic regression problem
#'
#' A function that will generate the task list LASSO logistic regression problem.
#'
#' @param lam Weight for the L1 penalty
#'
#' @return A list containing all components needed for benchmarking the problem
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Guyon I, Gunn SR, Ben-Hur A, Dror G (2004). Result analysis of the NIPS 2003 feature selection challenge. In: NIPS, volume 4, 545–552.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' problem = lasso_task(lam=1)
#' benchmark(
#'   problem,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 200
#' )
#' }
#'
#' @export lasso_task
lasso_task = function(lam){
  Madelon = AccelBenchmark::Madelon

  n = nrow(Madelon$X)
  p = ncol(Madelon$X)

  LL = norm(Madelon$X, "2")^2
  stplngth = 2 / LL

  fp = function(par, X, y){GDstep(par, X, y, lam, stplngth)}
  obj = function(par, X, y){LogisticLoss(par, X, y, lam)}
  list(
    initfn = function(){lasso_init(p)},
    fixptfn = fp,
    objfn = obj,
    X = Madelon$X,
    y = Madelon$y
  )
}

