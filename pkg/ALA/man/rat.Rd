\name{rat}
\alias{rat}
\docType{data}

% -------------------------------------------------------------------------

\title{Weight gain in rats exposed to thiouracil and thyroxin}


\description{

  The data are from a study of weight gain, where investigators randomly
  assigned 30 rats to three treatment groups.

}


\usage{data(rat)}


\format{
  
  A data frame with 135 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 27 levels}
    \item{\code{treatment}}{a factor with levels \code{control},
      \code{thiouracil}, and \code{thyroxin}}
    \item{\code{week}}{a numeric vector}
    \item{\code{weight}}{a numeric vector}
    
  }
  
}


\details{
  
  The data are from a study of weight gain, where investigators randomly
  assigned 30 rats to three treatment groups: treatment 1 was a control
  (no additive); treatments 2 and 3 consisted of two different additives
  (thiouracil and thyroxin respectively) to the rats drinking
  water.  Weight was measured at baseline (week 0) and at weeks 1, 2, 3,
  and 4.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{weight}.

  Due to an accident at the beginning of the study, data on 3 rats from
  the thyroxin group are unavailable.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Box GEP  (1950)  Problems in the analysis of growth and wear
  curves.  Biometrics 6:362-389

}

% -------------------------------------------------------------------------

\examples{

str(rat)
summary(rat)

}


\keyword{datasets}
