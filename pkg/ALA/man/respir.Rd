\name{respir}
\alias{respir}
\docType{data}

% -------------------------------------------------------------------------

\title{Clinical trial comparing two treatments for a respiratory
  illness}


\description{

  The data are from a clinical trial of patients with respiratory
  illness, where 111 patients from two different clinics were randomized
  to receive either placebo or an active treatment.

}


\usage{data(respir)}


\format{
  
  A data frame with 555 observations on the following 7 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 56 levels}
    \item{\code{gender}}{a factor with levels \code{F} and \code{M}}
    \item{\code{center}}{a factor with levels \code{C1} and \code{C2}}
    \item{\code{treatment}}{a factor with levels \code{active} and
      \code{placebo}}
    \item{\code{age}}{a numeric vector}
    \item{\code{visit}}{a numeric vector}
    \item{\code{status}}{a factor with levels \code{poor} and
      \code{good} (response)}
    
  }
  
}


\details{

  Patients were examined at baseline and at four visits during
  treatment. At each examination, respiratory status (categorized as 1 =
  good, 0 = poor) was determined.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{status}.  The center ID was prefixed with the letter \dQuote{C}
  in the \code{center} variable, compared to the source.
  
}


\section{Warning}{

  The original description says there are 111 patients, yet the data
  contain only 56 IDs.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Stokes ME, Davis CS, Koch GG (1995) Categorical Data Analysis using
  the SAS System. Cary, NC: SAS Institute, Inc.

}

% -------------------------------------------------------------------------

\examples{

str(respir)
summary(respir)

}


\keyword{datasets}
