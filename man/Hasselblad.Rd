% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hasselblad-data.R
\docType{data}
\name{Hasselblad}
\alias{Hasselblad}
\title{Hasselblad death count data}
\format{
A List where \code{"$death"} is an array for death numbers from 0 to 9, \code{"$freq"} contains counts for that specific death number.
}
\usage{
data(Hasselblad)
}
\description{
Data contains number of deaths of women 80 years and older during the years 1910-1912 from \emph{The London Times}.
}
\examples{
data(Hasselblad)
\donttest{plot(Hasselblad$death, Hasselblad$freq)}

}
\references{
Hasselblad V (1969)
(\href{https://www.tandfonline.com/doi/abs/10.1080/01621459.1969.10501071}{doi:10.1080/01621459.1969.10501071})
}
\keyword{datasets}
