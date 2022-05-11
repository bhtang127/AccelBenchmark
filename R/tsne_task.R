#############################################################
# Functions calculating the similarity matrix
.Hbeta <- function(D, beta){
  P = exp(-D * beta)
  sumP = sum(P)
  if (sumP == 0){
    H = 0
    P = D * 0
  } else {
    H = log(sumP) + beta * sum(D %*% P) /sumP
    P = P/sumP
  }

  list(H=H, P=P)
}

#' @importFrom stats dist
.x2p <- function(X, perplexity=30, tol=1e-5, eps=1e-15){
  if (inherits(X, 'dist')) {
    D = X
    n = attr(D,'Size')
  } else{
    X = as.matrix(X)
    X = X - min(X)
    X = X / max(X)
    D = dist(X)
    n = attr(D,'Size')
  }

  D = as.matrix(D)
  P = matrix(0, n, n)
  beta = rep(1, n)
  logU = log(perplexity)

  for (i in 1:n){
    betamin = -Inf
    betamax = Inf
    Di = D[i, -i]
    hbeta = .Hbeta(Di, beta[i])
    H = hbeta$H;
    thisP = hbeta$P
    Hdiff = H - logU;
    tries = 0;

    while(abs(Hdiff) > tol && tries < 50){
      if (Hdiff > 0){
        betamin = beta[i]
        if (is.infinite(betamax)) beta[i] = beta[i] * 2
        else beta[i] = (beta[i] + betamax)/2
      } else{
        betamax = beta[i]
        if (is.infinite(betamin))  beta[i] = beta[i]/ 2
        else beta[i] = ( beta[i] + betamin) / 2
      }

      hbeta = .Hbeta(Di, beta[i])
      H = hbeta$H
      thisP = hbeta$P
      Hdiff = H - logU
      tries = tries + 1
    }
    P[i,-i]  = thisP
  }

  P = .5 * (P + t(P))
  P[P < eps] = eps
  for(i in 1:nrow(P)) P[i, i] = 0
  P / sum(P)
}

.Qq = function(par){
  X = matrix(par, ncol=2)
  q = 1 / (1 + as.matrix(dist(X))^2)
  for(i in 1:nrow(q)) q[i, i] = 0
  Q = q / sum(q)
  list(Q=Q, q=q)
}

.Laplacian = function(M){
  diag(rowSums(M)) - M
}


## tSNE loss function
tsne_loss = function(par, P, ...){
  Qq = .Qq(par); ind = upper.tri(P)
  2 * sum(P[ind] * (log(P[ind]) - log(Qq$Q[ind])))
}

## MM algorithm for tSNE
tsne_update = function(par, P, rho, ...){
  Y = matrix(par, ncol=2); n = nrow(Y)
  Qq = .Qq(par)
  LPq = .Laplacian(P * Qq$q)
  # eigen.LPq = svd(LPq)

  # W = P * Qq$q
  # Psi = -4 * .Laplacian(Qq$Q * Qq$q) %*% Y
  # ind = upper.tri(P)
  # Lold = 2 * sum(P[ind] * (log(P[ind]) - log(Qq$Q[ind])))
  # const = Lold - sum(W * as.matrix(dist(Y))^2)

  LQq = .Laplacian(Qq$Q * Qq$q)

  # for(i in 1:3){
  Ynew = solve(LPq + rho / 4 * diag(n), LQq %*% Y + rho * Y / 4)
  #   G = const + sum(W * as.matrix(dist(Ynew))^2) +
  #     rho * sum((Ynew-Y)^2) / 2 + sum(Psi * (Ynew-Y))
  #   if(tsne_loss(as.vector(Ynew), P) < G)
  #     break
  #   rho = rho * 5
  # }
  as.vector(Ynew)
}

# init function for tSNE
#' @importFrom stats rnorm
tsne_init = function(n, sd=1e-4){
  sd * rnorm(n*2)
}


#' tSNE problem
#'
#' A function that will generate the task list that can be used to benchmark the acceleration of tSNE in COIL-20 data
#'
#' @param perplexity Perplexity value used to calculate the similarity matrix in original space before embedding
#' @param rho Initial Learning rate used in the MM algorithm for tSNE
#' @param sd Standard deviation for the random initial embedding coordinates
#'
#' @return A list containing all components needed for benchmarking the problem
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Yang Z, Peltonen J, Kaski S (2015). Majorization-minimization for manifold embedding. In: Artificial Intelligence and Statistics, 1088–1097. PMLR.
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' problem = tsne_task(rho=5e-6, sd=5e-3)
#' benchmark(
#'   problem,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 20
#' )
#' }
#'
#' @export tsne_task
tsne_task = function(perplexity=30, rho=2e-7, sd=1e-2){
  P = .x2p(AccelBenchmark::COIL_20$X, perplexity)
  init = function(){tsne_init(nrow(P), sd)}
  list(
    initfn = init,
    fixptfn = tsne_update,
    objfn = tsne_loss,
    P = P,
    rho = rho
  )
}
