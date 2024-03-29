% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BSDA-package.R
\docType{data}
\name{Aid}
\alias{Aid}
\title{Monthly payments per person for families in the AFDC federal program}
\format{
A data frame/tibble with 51 observations on two variables
\describe{ 
\item{state}{a factor with levels \code{Alabama},
\code{Alaska}, \code{Arizona}, \code{Arkansas}, \code{California},
\code{Colorado}, \code{Connecticut}, \code{Delaware}, \code{District of
Colunbia}, \code{Florida}, \code{Georgia}, \code{Hawaii}, \code{Idaho},
\code{Illinois}, \code{Indiana}, \code{Iowa}, \code{Kansas}, \code{Kentucky},
\code{Louisiana}, \code{Maine}, \code{Maryland}, \code{Massachusetts},
\code{Michigan}, \code{Minnesota}, \code{Mississippi}, \code{Missour},
\code{Montana}, \code{Nebraska}, \code{Nevada}, \code{New Hampshire}, \code{New
Jersey}, \code{New Mexico}, \code{New York}, \code{North Carolina}, \code{North
Dakota}, \code{Ohio}, \code{Oklahoma}, \code{Oregon}, \code{Pennsylvania},
\code{Rhode Island}, \code{South Carolina}, \code{South Dakota},
\code{Tennessee}, \code{Texas}, \code{Utah}, \code{Vermont}, \code{Virginia},
\code{Washington}, \code{West Virginia}, \code{Wisconsin}, and \code{Wyoming}}
\item{payment}{average monthly payment per person in a family} 
}
}
\source{
US Department of Health and Human Services, 1993.
}
\usage{
Aid
}
\description{
Data used in Exercises 1.91 and 3.68
}
\examples{

hist(Aid$payment, xlab = "payment", main = 
"Average monthly payment per person in a family", 
col = "lightblue")
boxplot(Aid$payment, col = "lightblue")
dotplot(state ~ payment, data = Aid)

}
\references{
Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}.
Pacific Grove, CA: Brooks/Cole, a division of Thomson Learning.
}
\keyword{datasets}
