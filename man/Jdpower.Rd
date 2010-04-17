\name{Jdpower}
\alias{Jdpower}
\docType{data}
\title{Number of problems reported per 100 cars in 1994 versus 1995s}
\description{
  Data for Exercise 2.14, 2.17, 2.31, 2.33, and 2.40
}
\usage{Jdpower}
\format{
  A data frame with 29 observations on the following 3 variables.
  \describe{
    \item{\code{Car}}{a factor with levels \code{Acura} \code{BMW} \code{Buick} \code{Cadillac} \code{Chevrolet} \code{Dodge} \code{Eagle} \code{Ford} \code{Geo} \code{Honda} \code{Hyundai} \code{Infiniti} \code{Jaguar} \code{Lexus} \code{Lincoln} \code{Mazda} \code{Mercedes-Benz} \code{Mercury} \code{Mitsubishi} \code{Nissan} \code{Oldsmobile} \code{Plymouth} \code{Pontiac} \code{Saab} \code{Saturn} \code{Subaru} \code{Toyota} \code{Volkswagen} \code{Volvo}}
    \item{\code{X1994}}{a numeric vector}
    \item{\code{X1995}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Jdpower)
attach(Jdpower)
plot(X1994,X1995)
model <- lm(X1995~X1994)
abline(model)
model
cor(X1995,X1994)
detach(Jdpower)
}
\keyword{datasets}
