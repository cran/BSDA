\name{Ticket}
\alias{Ticket}
\docType{data}
\title{Time to complete an airline ticket reservation }
\description{
  Data for Exercise 5.18
}
\usage{Ticket}
\format{
  A data frame with 20 observations on the following variable.
  \describe{
    \item{\code{time}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Ticket)
attach(Ticket)
EDA(time)
detach(Ticket)
}
\keyword{datasets}
