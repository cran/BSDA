\name{Kidsmoke}
\alias{Kidsmoke}
\docType{data}
\title{Smoking habits of boys and girls ages 12 to 18}
\description{
  Data for Exercise 4.85
}
\usage{Kidsmoke}
\format{
  A data frame with 1000 observations on the following 2 variables.
  \describe{
    \item{\code{gender}}{a numeric vector}
    \item{\code{smoke}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Kidsmoke)
attach(Kidsmoke)
table(gender,smoke)
addmargins(table(gender,smoke))
addmargins(table(gender,smoke)/1000)
detach(Kidsmoke)
}
\keyword{datasets}
