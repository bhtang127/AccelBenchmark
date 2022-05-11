#' @importFrom varbvs varbvsbin randn

### use 1 iteration `varbvsbin` as fixptfn
### update order uniformly sampled between 1:p and p:1
varbvs_update = function(par, X, y, sa, logodds, ...){
  if(length(logodds) == 1)
    logodds = rep(logodds, ncol(X))
  ps = bvs.par.split(par, ncol(X))
  update.order.sign = sample(1:2, 1)
  if(update.order.sign > 1)
    update.order = 1:ncol(X)
  else
    update.order = ncol(X):1
  res = varbvsbin(
    X=X, y=y, sa=sa, logodds=logodds,
    alpha=ps$alpha, mu=ps$mu, eta=ps$eta,
    update.order=update.order, tol=1e-15,
    maxiter=1, verbose=FALSE
  )
  return(c(c(res$alpha), c(res$mu), c(res$eta)))
}


bvs.par.split = function(par, nc){
  alpha = par[1:nc]
  mu = par[(1+nc):(nc+nc)]
  eta = par[(1+nc+nc):length(par)]
  list(alpha=alpha, mu=mu, eta=eta)
}


### varbvs loss function
varbvs_loss = function(par, X, y, sa, logodds, ...){
  if(length(logodds) == 1)
    logodds = rep(logodds, ncol(X))

  n <- nrow(X)
  p <- ncol(X)

  ps = bvs.par.split(par, p)

  Xr    <- c(X %*% (ps$alpha*ps$mu))
  stats <- updatestats_varbvsbin(X,y,ps$eta)
  s     <- sa/(sa*stats$xdx + 1)

  -1 * (int.logit(y,stats,ps$alpha,ps$mu,s,Xr,ps$eta) +
          int.gamma(logodds,ps$alpha) +
          int.klbeta(ps$alpha,ps$mu,s,sa))
}


# ----------------------------------------------------------------------
# Calculates useful quantities for updating the variational approximation
# to the logistic regression factors.
updatestats_varbvsbin <- function (X, y, eta) {

  # Compute the slope of the conjugate.
  d <- slope(eta)

  # Compute beta0 and yhat. See the journal paper for an explanation
  # of these two variables.
  beta0 <- sum(y - 0.5)/sum(d)
  yhat  <- y - 0.5 - beta0*d

  # Calculate xy = X'*yhat and xd = X'*d.
  xy <- c(yhat %*% X)
  xd <- c(d %*% X)

  # Compute the diagonal entries of X'*dhat*X. For a definition of
  # dhat, see the Bayesian Analysis journal paper.
  #
  # This is the less numerically stable version of this update:
  #
  #   xdx <- diagsq(X,d) - xd^2/sum(d)
  #
  dzr <- d/sqrt(sum(d))
  xdx <- diagsq(X,d) - c(dzr %*% X)^2

  # Return the result.
  return(list(d = d,yhat = yhat,xy = xy,xd = xd,xdx = xdx))
}

logpexp <- function (x) {
  y    <- x
  i    <- which(x < 16)
  y[i] <- log(1 + exp(x[i]))
  return(y)
}

sigmoid <- function (x)
  1/(1 + exp(-x))

logsigmoid <- function (x)
  -logpexp(-x)

slope <- function (x)
  (sigmoid(x) - 0.5) / x

diagsq = function (x, a)
  diag(t(x) %*% diag(a) %*% x)

dot <- function (x,y)
  sum(x*y)

# ----------------------------------------------------------------------
# Computes an integral that appears in the variational lower bound of
# the marginal log-likelihood for the logistic regression model. This
# integral is an approximation to the expectation of the logistic
# regression log-likelihood taken with respect to the variational
# approximation.
int.logit <- function (y, stats, alpha, mu, s, Xr, eta) {

  # Get some of the statistics.
  yhat <- stats$yhat
  xdx  <- stats$xdx
  d    <- stats$d

  # Get the variance of the intercept given the other coefficients.
  a <- 1/sum(d)

  # Compute the variational approximation to the expectation of the
  # log-likelihood with respect to the variational approximation.
  return(sum(logsigmoid(eta)) + dot(eta,d*eta - 1)/2 + log(a)/2 +
           a*sum(y - 0.5)^2/2 + dot(yhat,Xr) - quadnorm(Xr,d)^2/2 +
           a*dot(d,Xr)^2/2 - dot(xdx,betavar(alpha,mu,s))/2)
}

quadnorm <- function (x, a) {
  x <- c(x)
  if (is.matrix(a))
    y <- sqrt(c(x %*% a %*% x))
  else
    y <- sqrt(dot(x*a,x))
  return(y)
}

betavar <- function (p, mu, s) {
  p*(s + (1 - p)*mu^2)
}

int.gamma <- function (logodds, alpha)
  sum((alpha-1)*logodds + logsigmoid(logodds))

int.klbeta <- function (alpha, mu, s, sa, eps=1e-9)
  (sum(alpha) + dot(alpha,log(s/sa)) - dot(alpha,s + mu^2)/sa)/2 -
  dot(alpha,log(pmax(alpha, eps))) - dot(1 - alpha, log(pmax(1 - alpha, eps)))


varbvs.init = function(n, p, sd=1e-1){
  c(runif(p), rnorm(p)*sd, rnorm(n)*sqrt(sd))
}


varbvs.proj = function(p){
  function(par){
    par[1:p] = pmin(pmax(par[1:p], 1e-7), 1-1e-7)
    par
  }
}

#' @importFrom stats runif rnorm
varbvs.data = function(n, p, rate){
  maf <- 0.05 + 0.45*runif(p)
  X   <- (runif(n*p) < maf) + (runif(n*p) < maf)
  X   <- matrix(as.double(X),n,p,byrow = TRUE)
  Z   <- randn(n,2)

  u    <- c(-1,1)
  beta <- c(0.5*rnorm(floor(p*rate)),rep(0,p-floor(p*rate)))

  # Simulate the binary trait (case-control status) as a coin toss with
  # success rates given by the logistic regression.
  sigmoid <- function (x)
    1/(1 + exp(-x))
  y <- as.double(runif(n) < sigmoid(-1 + Z %*% u + X %*% beta))
  list(X=X, y=y)
}


#' Variational Bayes Variable Selection Problem
#'
#' A function that will generate the task list for variational Bayes variable selection problem.
#' Once ran, it will generate a simulated data that \eqn{logit(p_i) = -1 - Z_{i1} - Z_{i2} + X_i^T \beta},
#' where Z_i1, Z_i2 are independent standard normal variable. \eqn{\beta} are i.i.d follow Normal(0, 0.25) but some of them are set to be zero
#' X where drawn independently from Binomial(2, p) where p is uniform over (0.05, 0.5)
#'
#' @param n Number of rows in simulated data
#' @param p Number of predictors in simulated data
#' @param rate Only \code{floor(rate * p)} components in \eqn{\beta} will be non-zero
#' @param sd Standard deviation used in random initialize function
#'
#' @return A list containing all components needed for benchmarking the problem
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Carbonetto P, Stephens M, et al. (2012). Scalable variational inference for Bayesian variable selection in regression, and its accuracy in genetic association studies. Bayesian Analysis, 7(1): 73–108.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' problem = bvs_task(n=200, p=2000, rate=0.1)
#' benchmark(
#'   problem,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 200
#' )
#' }
#'
#' @export bvs_task
bvs_task = function(n=400, p=1000, sd=1e-1, rate=0.1){
  init = function() varbvs.init(n, p, sd)
  data = varbvs.data(n, p, rate)
  list(
    initfn = init,
    fixptfn = varbvs_update,
    objfn = varbvs_loss,
    X = data$X, y=data$y,
    sa = 0.5,
    logodds = log10(rate / (1-rate))
  )
}
