\name{Bumpers}
\alias{Bumpers}
\docType{data}
\title{Repair costs of vehicles crashed into a barrier at 5 miles per hour}
\description{
  Data for Exercise 1.73
}
\usage{Bumpers}
\format{
  A data frame with 23 observations on the following 2 variables.
  \describe{
    \item{\code{Car}}{a factor with levels \code{Buick Century} \code{Buick Skylark} \code{Chevrolet Cavalier} \code{Chevrolet Corsica} \code{Chevrolet Lumina} \code{Dodge Dynasty} \code{Dodge Monaco} \code{Ford Taurus} \code{Ford Tempo} \code{Honda Accord} \code{Hyundai Sonata} \code{Mazda 626} \code{Mitsubishi Galant} \code{Nissan Stanza} \code{Oldsmobile Calais} \code{Oldsmobile Ciere} \code{Plymouth Acclaim} \code{Pontiac 6000} \code{Pontiac Grand Am} \code{Pontiac Sunbird} \code{Saturn SL2} \code{Subaru Legacy} \code{Toyota Camry}}
    \item{\code{repair}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Bumpers)
attach(Bumpers)
EDA(repair)
sum(repair>(mean(repair)-sd(repair)) & 
repair < (mean(repair)+sd(repair)))/length(repair)
stripchart(repair,method="stack")
library(lattice)
dotplot(Car~repair)
detach(Bumpers)
}
\keyword{datasets}
