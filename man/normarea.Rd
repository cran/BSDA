\name{normarea}
\alias{normarea}
\title{Normal Area}
\description{Function that computes and draws the area between two user specified 
values in a user specified normal distribution with a given mean and standard deviation}
\usage{
normarea(lower = -Inf, upper = Inf, m, sig)
}

\arguments{
  \item{lower}{the lower value}
  \item{upper}{the upper value}
  \item{m}{the mean for the population}
  \item{sig}{the standard deviation of the population}
}

\author{Alan T. Arnholt}

\examples{
normarea(70, 130, 100, 15)
    # Finds and P(70 < X < 130) given X is N(100,15).
}
\keyword{distribution}
