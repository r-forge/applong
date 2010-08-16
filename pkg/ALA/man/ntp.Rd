\name{ntp}
\alias{ntp}
\docType{data}

% -------------------------------------------------------------------------

\title{Developmental toxicity study of ethylene glycol (EG)}


\description{

  The data are from a development toxicity study of ethylene glycol
  (EG).  Ethylene glycol is a high-volume industrial chemical used in
  many applications.

}


\usage{data(ntp)}


\format{
  
  A data frame with 1028 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor 94 with levels}
    \item{\code{dose}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
    \item{\code{malformed}}{a factor with levels \code{no} and
      \code{yes} (evidence of malformations)}
    
  }
  
}


\details{

  In a study of laboratory mice conducted through the National
  Toxicology Program (NTP), EG was administered at doses of 0, 750,
  1500, or 3000 mg/kg/day to 94 pregnant mice (dams) over the period of
  major organogenesis, beginning just after implantation.  Following
  sacrifice, fetal weight and evidence of malformations were recorded.

}


\note{

  Original variable names have been adapted to R conventions.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Price CJ, Kimmel CA, Tyl RW, Marr MC (1985) The developmental toxicity
  of ethylene glycol in rats and mice.  Toxicological Applications in
  Pharmacology 81:113-127

}

% -------------------------------------------------------------------------

\examples{

str(ntp)
summary(ntp)

}


\keyword{datasets}
