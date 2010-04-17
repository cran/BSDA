\name{Wins}
\alias{Wins}
\docType{data}
\title{ Baseball team wins versus 7 independent variables for National league teams in 1990}
\description{
  Data for Exercise 9.23
}
\usage{Wins}
\format{
  A data frame with 12 observations on the following 9 variables.
  \describe{
    \item{\code{team}}{a factor with levels \code{Atlanta} \code{Chicago} \code{Cincinnati} \code{Houston} \code{Los Angeles} \code{Montreal} \code{New York} \code{Philadelphia} \code{Pittsburgh} \code{San Diego} \code{San Francisco} \code{St. Louis}}
    \item{\code{wins}}{a numeric vector}
    \item{\code{batavg}}{a numeric vector}
    \item{\code{rbi}}{a numeric vector}
    \item{\code{stole}}{a numeric vector}
    \item{\code{strkout}}{a numeric vector}
    \item{\code{caught}}{a numeric vector}
    \item{\code{errors}}{a numeric vector}
    \item{\code{era}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Wins)
attach(Wins)
plot(era,wins)
model <- lm(wins~era)
abline(model)
summary(model)
detach(Wins)
remove(model)
}
\keyword{datasets}
