#' COIL-20 objects data
#'
#' Data contains 1440 128x128 images for 20 objects picturing from different angles.
#'
#' @docType data
#'
#' @usage data(COIL_20)
#'
#' @format A List where \code{"$X"} is an (1440, 16384) matrix contains all raster images and \code{"$label"} is object label for each image.
#'
#' @keywords datasets
#'
#' @references "Columbia Object Image Library (COIL-20)," S. A. Nene, S. K. Nayar and H. Murase, Technical Report CUCS-005-96, February 1996.
#'
#' @source \href{https://www.cs.columbia.edu/CAVE/software/softlib/coil-20.php}{COIL-20}
#'
#' @examples
#' data(COIL_20)
#' \donttest{image(matrix(COIL_20$X[1,], 128, 128), useRaster=TRUE, axes=FALSE, col=gray.colors(33))}
#'
"COIL_20"
