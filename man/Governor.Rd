% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Governor}
\alias{Governor}
\title{Annual salaries for state governors in 1994 and 1999}
\format{
A data frame/tibble with 50 observations on three variables
\describe{ 
\item{state}{a character variable with values \code{Alabama},
\code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
\code{Colorado}, \code{Connecticut}, \code{Delaware}, \code{Florida},
\code{Georgia}, \code{Hawaii}, \code{Idaho}, \code{Illinois}, \code{Indiana},
\code{Iowa}, \code{Kansas}, \code{Kentucky}, \code{Louisiana}, \code{Maine},
\code{Maryland}, \code{Massachusetts}, \code{Michigan}, \code{Minnesota},
\code{Mississippi}, \code{Missouri}, \code{Montana}, \code{Nebraska},
\code{Nevada}, \code{New Hampshire}, \code{New Jersey}, \code{New Mexico},
\code{New York}, \code{North Carolina}, \code{North Dakota}, \code{Ohio},
\code{Oklahoma}, \code{Oregon}, \code{Pennsylvania}, \code{Rhode Island},
\code{South Carolina}, \code{South Dakota}, \code{Tennessee}, \code{Texas},
\code{Utah}, \code{Vermont}, \code{Virginia}, \code{Washington}, \code{West
Virginia}, \code{Wisconsin}, and \code{Wyoming}} 
\item{year}{a factor indicating year} 
\item{salary}{a numeric vector with the governor's salary (in dollars)} 
}
}
\source{
\emph{The 2000 World Almanac and Book of Facts}.
}
\usage{
Governor
}
\description{
Data for Exercise 5.112
}
\examples{

boxplot(salary ~ year, data = Governor)

\dontrun{
library(ggplot2)
ggplot2::ggplot(data = Governor, aes(x = salary)) + 
           geom_density(fill = "pink") + 
           facet_grid(year ~ .) + 
           theme_bw()
}

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
