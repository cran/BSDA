\name{Superbowl}
\alias{Superbowl}
\docType{data}
\title{ Margin of victory in Superbowls I to XXXV}
\description{
  Data for Exercise 1.54
}
\usage{Superbowl}
\format{
  A data frame with 35 observations on the following 5 variables.
  \describe{
    \item{\code{Winning.team}}{a factor with levels \code{Baltimore Colts} \code{Baltimore Ravens} \code{Chicago Bears} \code{Dallas Cowboys} \code{Denver Broncos} \code{Green Bay Packers} \code{Kansas City Chiefs} \code{Los Angeles Raiders} \code{Miami Dolphins} \code{New York Giants} \code{New York Jets} \code{Oakland Raiders} \code{Pittsburgh Steelers} \code{San Francisco 49ers} \code{St Louis Rams} \code{Washington Redskins}}
    \item{\code{winner.score}}{a numeric vector}
    \item{\code{Losing.team}}{a factor with levels \code{Atlanta Falcons} \code{Baltimore Colts} \code{Buffalo Bills} \code{Cincinnati Bengals} \code{Dallas Cowboys} \code{Denver Broncos} \code{Green Bay Packers} \code{Kansas City Chiefs} \code{Los Angeles Rams} \code{Miami Dolphins} \code{Minnesota Vikings} \code{New England Patriots} \code{New York Giants} \code{Oakland Raiders} \code{Philadelphia Eagles} \code{Pittsburgh Steelers} \code{San Diego Chargers} \code{Tennessee Titans} \code{Washington Redskins}}
    \item{\code{loser.score}}{a numeric vector}
    \item{\code{margin}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Superbowl)
attach(Superbowl)
stem(margin)
detach(Superbowl)
}
\keyword{datasets}
