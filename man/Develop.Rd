\name{Develop}
\alias{Develop}
\docType{data}
\title{Demographic characteristics of developmental students at 2-year colleges and 4-year colleges}
\description{
 Data used for Exercise 8.50
}
\usage{Develop}
\format{
  The format is:
 num [1:5, 1:2]  545   24   71  142 1587 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:5] "African America" "American Indian" "Asian" "Latino" ...
  ..$ : chr [1:2] "Two-year" "Four-year"
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
Develop
chisq.test(Develop)
}
\keyword{datasets}
