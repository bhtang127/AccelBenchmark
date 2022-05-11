###################################################
## EM algorithm
#' @importFrom BB dfsane
mvt.df.em <- function(par, y){
  # EM algorithm for multivariate t-distribution
  # unknown degrees of freedom
  # accepts a "vector" input and returns a "vector"
  m <- ncol(y)
  n <- nrow(y)
  nu <- par[1]
  mu <- par[2:(m+1)]
  V <- matrix(par[-(1:(m+1))],m,m)
  #d <- apply(t(t(y) - mu), 1, A = V, function(x,A) t(x) %*% solve(A) %*% x)
  d <- apply(t(t(y) - mu), 1, A = V, function(x,A) crossprod(x, solve(A, x)))
  w <- (nu + m) / (nu + d)

  phi <- function(x) digamma(x) -log(x)
  phiv2 <- function(x) phi(x/2) - phi((nu+m)/2) + mean(w-log(w)-1)

  nu.obj <- dfsane(fn=phiv2, par=nu, quiet=TRUE, control=list(trace=FALSE))
  nu <- nu.obj$par

  mu <- as.vector(t(y) %*% w / sum(w))
  sigma <-  array(apply(t(t(y)-mu),1,function(x){outer(x,x)}),dim=c(m,m,n))
  sigma <- apply(sigma,c(1,2),w=w,function(x,w) mean(x*w))

  return(c(nu, c(mu), as.numeric(sigma)))
}


###################################################
## MMF algorithm
#' @importFrom BB dfsane
mvt.mmf <- function(par, y){
  # a new algorithm
  # accepts a "vector" input and returns a "vector"
  m <- ncol(y)
  n <- nrow(y)
  nu <- par[1]
  mu <- par[2:(m+1)]
  V <- matrix(par[-(1:(m+1))],m,m)
  #d <- apply(t(t(y) - mu), 1, A = V, function(x,A) t(x) %*% solve(A) %*% x)
  d <- apply(t(t(y) - mu), 1, A = V, function(x,A) crossprod(x, solve(A, x)))
  w <- (nu + m) / (nu + d)

  mu <- as.vector(t(y) %*% w / sum(w))
  sigma <-  array(apply(t(t(y)-mu),1,function(x){outer(x,x)}),dim=c(m,m,n))
  sigma <- apply(sigma,c(1,2),w=w,function(x,w) sum(x*w)) / sum(w)

  d1 <- apply(t(t(y) - mu), 1, A = sigma, function(x,A) crossprod(x, solve(A, x)))

  phi <- function(x) digamma(x) -log(x)
  phiv2 <- function(x) phi(x/2) - phi((x+m)/2) + mean((nu+m)/(nu+d1)-log((nu+m)/(nu+d1))-1)
  nu.obj <- dfsane(fn=phiv2, par=nu, quiet=TRUE, control=list(trace=FALSE))
  nu <- nu.obj$par

  return(c(nu, mu,as.numeric(sigma)))
}

###################################################
mvt.df.mloglik <- function(par, y) {
  m <- ncol(y)
  n <- nrow(y)
  nu <- par[1]
  mu <- par[2:(m+1)]
  V <- matrix(par[-(1:(m+1))],m,m)
  d <- apply(t(t(y) - mu), 1, A = V, function(x,A) crossprod(x, solve(A, x)))
  mloglik <- -2*lgamma((m+nu)/2) + 2*lgamma(nu/2) - nu*log(nu) +
    log(det(V)) + (nu+m)*mean(log(nu+d))
  return(mloglik)
}


####################################################################
## Data simulation function
#' @importFrom MASS mvrnorm
#' @importFrom stats rchisq
rmvt.sim <- function(n, mu, V, df){
  # simulates random variables from a multivariate t-distribution
  p <- length(mu)
  y <- mvrnorm(n,mu=mu,Sigma=V) / sqrt(rchisq(n,df=df)/df)
  return(y)
}


ndim = 2
n = 1000

##########################################################################3
## init function
mvt_init = function(mvt_df){
  ind = sample(n, n, replace=TRUE)
  y = mvt_df[ind,]
  nu0 <- sample(c(2, 3, 4), 1)
  mu0 <- apply(y, 2, mean)
  V0 <-  array(apply(t(t(y) - mu0), 1, function(x){outer(x, x)}),
               dim=c(ndim, ndim, n))
  V0 <- apply(V0, c(1, 2), function(x) mean(x))

  c(nu0, mu0, V0)
}


#' Multivarite t distribution problem
#'
#' A function that will generate the task list for problem of estimating parameters of a multivariate t distribution.
#' We would simulate 1000 2-dimensional t distribution data with mean 0 and \eqn{\Sigma = diag(0.1,1)}.
#' And the base fixed point algorithm is MMF in Hasannasab (2021).
#'
#' @param df Degree of freedom for the t-distribution
#'
#' @return A list containing all components needed for benchmarking the problem
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Hasannasab M, Hertrich J, Laus F, Steidl G (2021). Alternatives to the EM algorithm for ML estimation of location, scatter matrix, and degree of freedom of the Student t distribution. Numerical Algorithms, 87(1): 77–118.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' problem = mvt_mmf_task(df=25)
#' benchmark(
#'   problem,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 200
#' )
#' }
#'
#' @export mvt_mmf_task
mvt_mmf_task = function(df){
  mu = rep(0, ndim)
  V = diag(c(0.1, 1))
  mvt_df = rmvt.sim(n, mu, V, df)
  list(
    initfn = function() mvt_init(mvt_df),
    fixptfn = mvt.mmf,
    objfn = mvt.df.mloglik,
    y = mvt_df
  )
}
