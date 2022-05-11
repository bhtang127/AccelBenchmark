#' Hasselblad death count data
#'
#' Data contains number of deaths of women 80 years and older during the years 1910-1912 from \emph{The London Times}.
#'
#' @docType data
#'
#' @usage data(Hasselblad)
#'
#' @format A List where \code{"$death"} is an array for death numbers from 0 to 9, \code{"$freq"} contains counts for that specific death number.
#'
#' @keywords datasets
#'
#' @references Hasselblad V (1969)
#' (\href{https://www.tandfonline.com/doi/abs/10.1080/01621459.1969.10501071}{doi:10.1080/01621459.1969.10501071})
#'
#' @examples
#' data(Hasselblad)
#' \donttest{plot(Hasselblad$death, Hasselblad$freq)}
#'
"Hasselblad"
