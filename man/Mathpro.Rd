\name{Mathpro}
\alias{Mathpro}
\docType{data}
\title{Math proficiency and SAT scores by states}

\description{Data for Exercise 9.24, Example 9.1, and Example 9.6}

\usage{Mathpro}

\format{
  A data frame with 51 observations on the following 10 variables.
  \describe{
    \item{\code{state1}}{a factor with levels \code{} \code{Conn} \code{D.C.} \code{Del} \code{Ga} \code{Hawaii} \code{Ind} \code{Maine} \code{Mass} \code{Md} \code{N.C.} \code{N.H.} \code{N.J.} \code{N.Y.} \code{Ore} \code{Pa} \code{R.I.} \code{S.C.} \code{Va} \code{Vt}}
    \item{\code{Sat.M1}}{a numeric vector}
    \item{\code{Profic1}}{a numeric vector}
    \item{\code{state2}}{a factor with levels \code{} \code{Ala} \code{Alaska} \code{Ariz} \code{Ark} \code{Calif} \code{Colo} \code{Fla} \code{Idaho} \code{Ill} \code{Iowa} \code{Kan} \code{Ky} \code{La} \code{Mich} \code{Minn} \code{Miss} \code{Mo} \code{Mont} \code{N.D.} \code{N.M.} \code{Neb} \code{Nev} \code{Ohio} \code{Okla} \code{S.D.} \code{Tenn} \code{Texas} \code{Utah} \code{W.V.} \code{Wash} \code{Wis} \code{Wyo}}
    \item{\code{Sat.M2}}{a numeric vector}
    \item{\code{Profic2}}{a numeric vector}
    \item{\code{state}}{a factor with levels \code{Ala} \code{Alaska} \code{Ariz} \code{Ark} \code{Calif} \code{Colo} \code{Conn} \code{D.C.} \code{Del} \code{Fla} \code{Ga} \code{Hawaii} \code{Idaho} \code{Ill} \code{Ind} \code{Iowa} \code{Kan} \code{Ky} \code{La} \code{Maine} \code{Mass} \code{Md} \code{Mich} \code{Minn} \code{Miss} \code{Mo} \code{Mont} \code{N.C.} \code{N.D.} \code{N.H.} \code{N.J.} \code{N.M.} \code{N.Y.} \code{Neb} \code{Nev} \code{Ohio} \code{Okla} \code{Ore} \code{Pa} \code{R.I.} \code{S.C.} \code{S.D.} \code{Tenn} \code{Texas} \code{Utah} \code{Va} \code{Vt} \code{W.V.} \code{Wash} \code{Wis} \code{Wyo}}
    \item{\code{Sat.M}}{a numeric vector}
    \item{\code{Profic}}{a numeric vector}
    \item{\code{Group}}{a numeric vector}
  }
}

\source{Kitchens, L. J. (2003) \emph{Basic Statistics and Data Analysis}. Duxbury}

\examples{
str(Mathpro)
attach(Mathpro)
model <- lm(Sat.M1~Profic1)
plot(Profic1,Sat.M1)
abline(model)
model
detach(Mathpro)
remove(model)
}
\keyword{datasets}
