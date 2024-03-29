% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Nervous}
\alias{Nervous}
\title{Reaction effects of 4 drugs on 25 subjects with a nervous disorder}
\format{
A data frame/tibble with 25 observations on two variables
\describe{ 
\item{react}{a numeric vector representing reaction time} 
\item{drug}{a numeric vector indicating each of the 4 drugs} 
}
}
\usage{
Nervous
}
\description{
Data for Example 10.3
}
\examples{

boxplot(react ~ drug, data = Nervous, col = rainbow(4))
model <- aov(react ~ factor(drug), data = Nervous)
summary(model)
TukeyHSD(model)
plot(TukeyHSD(model), las = 1)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
