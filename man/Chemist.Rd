\name{Chemist}
\alias{Chemist}
\docType{data}
\title{Starting salaries of 50 chemistry majors}
\description{
  Data for Example 5.3 
}
\usage{Chemist}
\format{
  A data frame with 50 observations on the following variable.
  \describe{
    \item{\code{salary}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Chemist)
attach(Chemist)
EDA(salary)
detach(Chemist)
}
\keyword{datasets}
