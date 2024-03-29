% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Housing}
\alias{Housing}
\title{Median home prices for 1984 and 1993 in 37 markets across the U.S.}
\format{
A data frame/tibble with 74 observations on three variables
\describe{ 
\item{city}{a character variable with values \code{Albany},
\code{Anaheim}, \code{Atlanta}, \code{Baltimore}, \code{Birmingham},
\code{Boston}, \code{Chicago}, \code{Cincinnati}, \code{Cleveland},
\code{Columbus}, \code{Dallas}, \code{Denver}, \code{Detroit}, \code{Ft
Lauderdale}, \code{Houston}, \code{Indianapolis}, \code{Kansas City}, \code{Los
Angeles}, \code{Louisville}, \code{Memphis}, \code{Miami}, \code{Milwaukee},
\code{Minneapolis}, \code{Nashville}, \code{New York}, \code{Oklahoma City},
\code{Philadelphia}, \code{Providence}, \code{Rochester}, \code{Salt Lake City},
\code{San Antonio}, \code{San Diego}, \code{San Francisco}, \code{San Jose},
\code{St Louis}, \code{Tampa}, and \code{Washington}} 
\item{year}{a factor with levels \code{1984} and \code{1993}} 
\item{price}{median house price (in dollars)} 
}
}
\source{
National Association of Realtors.
}
\usage{
Housing
}
\description{
Data for Exercise 5.82
}
\examples{

stripchart(price ~ year, data = Housing, method = "stack", 
           pch = 1, col = c("red", "blue"))
\dontrun{
library(ggplot2)
ggplot2::ggplot(data = Housing, aes(x = price, fill = year)) + 
           geom_dotplot() + 
           facet_grid(year ~ .) + 
           theme_bw()
}               

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
