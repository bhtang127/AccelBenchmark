#' Madelon data
#'
#' Simulated data with highly ill-conditioned design matrix.
#'
#' @docType data
#'
#' @usage data(Madelon)
#'
#' @format A List where \code{"$X"} is design matrix of size (2600, 500), \code{"$y"} is the binary outcome for each row.
#'
#' @keywords datasets
#'
#' @references Guyon, Isabelle, et al. "Result analysis of the nips 2003 feature selection challenge." Advances in neural information processing systems 17 (2004).
#' (\href{https://proceedings.neurips.cc/paper/2004/file/5e751896e527c862bf67251a474b3819-Paper.pdf}{NeurIPS})
#'
#' @examples
#' data(Madelon)
#' \donttest{table(Madelon$y)}
#'
"Madelon"
