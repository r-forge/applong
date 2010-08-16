\name{chd}
\alias{chd}
\docType{data}

% -------------------------------------------------------------------------

\title{Risk factors for coronary heart disease (CHD)}


\description{

  The data arise from a prospective study of potential risk factors for
  coronary heart disease (CHD).

}


\usage{data(chd)}


\format{

  A data frame with 16 observations on the following 5 variables.
  
  \describe{

    \item{\code{smoking}}{a factor with levels \code{0}, \code{10},
      \code{20}, and \code{30}}
    \item{\code{blood.pressure}}{a factor with levels \code{< 140} and
      \code{>= 140}}
    \item{\code{behaviour}}{a factor with levels \code{B} and \code{A}}
    \item{\code{nCHD}}{a numeric vector; count of Cases of CHD}
    \item{\code{years.followup}}{a numeric vector}
    
  }
  
}


\details{

  The study observed 3154 men aged 40-50 for an average of 8 years and
  recorded the incidence of cases of CHD. The potential risk factors
  included smoking, blood pressure, and personality/behaviour type.

}


\note{

  Original variable names have been adapted to R conventions.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Selvin S (1995) Practical Biostatistical Methods.  Belmont, CA:
  Wadsworth Publishing Company.

}

% -------------------------------------------------------------------------

\examples{

str(chd)
summary(chd)

}


\keyword{datasets}
