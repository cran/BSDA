\name{Brain}
\alias{Brain}
\docType{data}
\title{Brain weight versus body weight of 28 animals}
\description{
  Data for Exercises 2.15, 2.44, 2.58 and Examples 2.3 and 2.20 
}
\usage{Brain}
\format{
  A data frame with 28 observations on the following 5 variables.
  \describe{
    \item{\code{Species}}{a factor with levels \code{Africian elephant} \code{Asian Elephant} \code{Brachiosaurus} \code{Cat} \code{Chimpanzee} \code{Cow} \code{Diplodocus} \code{Donkey} \code{Giraffe} \code{Goat} \code{Gorilla} \code{Gray wolf} \code{Guinea Pig} \code{Hamster} \code{Horse} \code{Human} \code{Jaguar} \code{Kangaroo} \code{Mole} \code{Mouse} \code{Mt Beaver} \code{Pig} \code{Potar monkey} \code{Rabbit} \code{Rat} \code{Rhesus monkey} \code{Sheep} \code{Triceratops}}
    \item{\code{body.wt}}{a numeric vector}
    \item{\code{brain.wt}}{a numeric vector}
    \item{\code{logbody}}{a numeric vector}
    \item{\code{logbrain}}{a numeric vector}
  }
}
\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Brain)
attach(Brain)
plot(logbody,logbrain,pch=19,col="blue",main="Example 2.3")
model <- lm(logbrain~logbody)
abline(model)
detach(Brain)
}
\keyword{datasets}
