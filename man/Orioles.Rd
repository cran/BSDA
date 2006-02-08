\name{Orioles}
\alias{Orioles}
\docType{data}
\title{Salaries of members of the Baltimore Orioles baseball team}
\description{
  Data for Example 1.3
}
\usage{Orioles}
\format{
  A data frame with 27 observations on the following 3 variables.
  \describe{
    \item{\code{first.name}}{a factor with levels \code{Albert} \code{Arthur} \code{B.J.} \code{Brady} \code{Cal} \code{Charles} \code{dl-Delino} \code{dl-Scott} \code{Doug} \code{Harold} \code{Heathcliff} \code{Jeff} \code{Jesse} \code{Juan} \code{Lenny} \code{Mike} \code{Rich} \code{Ricky} \code{Scott} \code{Sidney} \code{Will} \code{Willis}}
    \item{\code{last.name}}{a factor with levels \code{Amaral} \code{Anderson} \code{Baines} \code{Belle} \code{Bones} \code{Bordick} \code{Clark} \code{Conine} \code{Deshields} \code{Erickson} \code{Fetters} \code{Garcia} \code{Guzman} \code{Johns} \code{Johnson} \code{Kamieniecki} \code{Mussina} \code{Orosco} \code{Otanez} \code{Ponson} \code{Reboulet} \code{Rhodes} \code{Ripken Jr.} \code{Slocumb} \code{Surhoff} \code{Timlin} \code{Webster}}
    \item{\code{X1999salary}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}
\examples{
str(Orioles)
attach(Orioles)
stripchart(X1999salary,method="stack",pch=19,col="blue")
detach(Orioles)
}
\keyword{datasets}
