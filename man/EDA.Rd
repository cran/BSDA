\name{EDA}
\alias{EDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Exploratory Data Anaalysis}
\description{
Function that produces a histogram, density plot, boxplot, and Q-Q plot. 
}
\usage{
EDA(x, trim = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector. \code{NA}s and \code{Inf}s are allowed but will be removed. 
}
  \item{trim}{fraction (between 0 and 0.5, inclusive) of values to be trimmed from each end of the ordered data. 
  If \code{trim = 0.5}, the result is the median. 
}
}
\details{
 Will not return command window information on data sets containing more than 5000 observations. 
 It will however still produce graphical output for data sets containing more than 5000 
 observations.  
}
\value{
  Function returns various measures of center and location. The values returned for the 
  Quartiles are based on the definitions provided in \cite{BSDA}. The boxplot is based on the 
  Quartiles returned in the commands window. 

}

\author{Alan T. Arnholt}
\note{Requires package \pkg{e1071}.}

\examples{
EDA(rnorm(100))
    # Produces four graphs for the 100 randomly
    # generated standard normal variates.
}
\keyword{univar} 
