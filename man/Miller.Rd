\name{Miller}
\alias{Miller}
\docType{data}
\title{Miller personality test scores for a group of college students applying for graduate school}
\description{
  Data for Example 5.7
}
\usage{Miller}
\format{
  A data frame with 25 observations on the following variable.
  \describe{
    \item{\code{miller}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Miller)
attach(Miller)
stem(miller)
fivenum(miller)
boxplot(miller)
qqnorm(miller,col="blue")
qqline(miller,col="red")
detach(Miller)
}
\keyword{datasets}
