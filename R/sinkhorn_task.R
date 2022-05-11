#####################################################################
## We'd like to scale a matrix G that finding
##    P = diag(u) G diag(v)
##     s.t.  rowsum(P) = \alpha
##           colsum(P) = \beta
## Original Sinkhorn Iteration algorithm is
##    u_{k+1} = \alpha / G v_k
##    v_{k+1} = \beta / G^T u_k
#####################################################################


## Sinkhorn algorithm
sinkhorn_update = function(par, G, a, b){
  # u = par[1:nrow(G)]
  # v = par[(nrow(G)+1):length(par)]

  # unew = a / c(G %*% v)
  unew = a / c(G %*% par)
  vnew = b / c(t(G) %*% unew)

  # c(unew, vnew)
  vnew
}


## loss function
sinkhorn_loss = function(par, G, a, b){
  # u = par[1:nrow(G)]
  # v = par[(nrow(G)+1):length(par)]
  u = a / c(G %*% par)
  # G = t(t(u * G) * v)
  G = t(t(u * G) * par)
  rowfactor = a / rowSums(G)
  colfactor = b / colSums(G)

  max(mean(abs(1-rowfactor)),
      mean(abs(1-colfactor)))
}


## init function
sinkhorn_init = function(n, m){
  runif(m, min=0.5, max=2)
}


#' Matrix balancing problem
#'
#' A function that will generate the task list that can be used to benchmark acceleration of Sinkhorn iteration in Matrix balancing problem
#'
#' @param mat Matrix to balancing. Can be a matrix or character name "Hessenberg" or "Marshall_Olkin" for specific matrices with those names.
#' @param order Order of the matrix. Only meaningful when \code{mat} argument is "Hessenberg"
#'
#' @return A list containing all components needed for benchmarking the problem
#'  \item{initfn}{Parameter random initializing function}
#'  \item{fixptfn}{Updating function for the fixed point iteration problem}
#'  \item{objfn}{Function calculating the objective value for current parameter}
#'  \item{...}{Other arguments required in functions above}
#'
#' @references Sinkhorn R, Knopp P (1967). Concerning nonnegative matrices and doubly stochastic matrices. Pacific Journal of Mathematics, 21(2): 343–348.
#' @references Parlett B, Landis TL (1982). Methods for scaling to doubly stochastic form. Linear Algebra and its Applications, 48: 53–79
#'
#' @examples
#' \dontrun{
#' set.seed(54321)
#' problem = sinkhorn_task(mat="Hessenberg", order=25)
#' benchmark(
#'   problem,
#'   algorithm=c("raw", "squarem", "daarem", "pem", "qn", "nes"),
#'   ntimes = 200
#' )
#' }
#'
#' @export sinkhorn_task
sinkhorn_task = function(mat="Marshall_Olkin", order=20){
  if(inherits(mat, "character")){
    if(startsWith(tolower(mat), "mar")){
      A = matrix(c(100,100,0,100,10000,1,0,1,100), 3, 3)
    }
    else{
      p <- order
      A <- matrix(1, p, p)
      for (j in 1:(p-2)){
        A[(j+2):p, j] <- 0
      }
      diag(A) <- 100
    }
  } else if(inherits(mat, "matrix")){
    A = mat
  }
  init = function(){sinkhorn_init(nrow(A), ncol(A))}
  list(
    initfn = init,
    fixptfn = sinkhorn_update,
    objfn = sinkhorn_loss,
    G = A, a = rep(1, nrow(A)), b = rep(1, nrow(A))
  )
}
