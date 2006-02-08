\name{Combinations}
\alias{Combinations}
\title{Combinations}
\description{
Computes all possible combinations of \code{n} objects taken \code{k} at a time.
}
\usage{
Combinations(n, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{a number.}
  \item{k}{a number less than or equal to \code{n}.}
}

\value{Returns a matrix containing the possible combinations of 
\code{n} objects taken \code{k} at a time. 
}

\seealso{\code{\link{SRS}}}

\author{Tim Hesterberg}

\examples{
Combinations(5,2)
    # The columns in the matrix list the values of the 10 possible
    # combinations of 5 things taken 2 at a time.
}
\keyword{distribution}
