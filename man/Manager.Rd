\name{Manager}
\alias{Manager}
\docType{data}
\title{Advertised salaries offered general managers of major corporations in 1995}
\description{
  Data for Exercise 5.81
}
\usage{Manager}
\format{
  A data frame with 26 observations on the following variable.
  \describe{
    \item{\code{salary}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Manager)
attach(Manager)
stem(salary)
SIGN.test(salary)
detach(Manager)
}
\keyword{datasets}
