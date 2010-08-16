\name{muscatine}
\alias{muscatine}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on Obesity from the Muscatine Coronary Risk Factor Study}


\description{

  The data are from the Muscatine Coronary Risk Factor (MCRF) study, a
  longitudinal survey of school-age children in Muscatine, Iowa.

}


\usage{data(muscatine)}

\format{
  
  A data frame with 14568 observations on the following 6 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 4856 levels}
    \item{\code{gender}}{a factor with levels \code{M} and \code{F}}
    \item{\code{age0}}{a numeric vector; initial age}
    \item{\code{age}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
    \item{\code{obesity}}{a factor with levels \code{non-obese} and
      \code{obese}}
    
  }
  
}


\details{

  The MCRF study had the goal of examining the development and
  persistence of risk factors for coronary disease in children.  In the
  MCRF study, weight and height measurements of five cohorts of
  children, initially aged 5-7, 7-9, 9-11, 11-13, and 13-15 years, were
  obtained biennially from 1977 to 1981.  Data were collected on 4856
  boys and girls.  On the basis of a comparison of their weight to
  age-gender specific norms, children were classified as obese or not
  obese.

}


\note{

  Original variable names have been adapted to R conventions.  Variable
  \code{Occasion} in the source was recoded to \code{year}, as that is
  what it represents.

  Age denotes mid-point of age-cohort.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Woolson RF, Clarke WR (1984) Analysis of categorical incomplete
  longitudinal data.  Journal of the Royal Statistical Society, Series A
  147:87-99.

}

% -------------------------------------------------------------------------

\examples{

data(muscatine)
str(muscatine)

}


\keyword{datasets}
