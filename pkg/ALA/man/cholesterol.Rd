\name{cholesterol}
\alias{cholesterol}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on serum cholesterol from the National Cooperative Gallstone
  Study (NCGS)}


\description{

  The data are from the National Cooperative Gallstone Study (NCGS),
  where one of the major interests was to study the safety of the drug
  chenodiol for the treatment of cholesterol gallstones

}


\usage{data(cholesterol)}


\format{

  A data frame with 515 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 65 levels}
    \item{\code{treatment}}{a factor with levels \code{high chenodiol}
      and \code{placebo}}
    \item{\code{month}}{a numeric vector}
    \item{\code{cholesterol}}{a numeric vector}

  }
  
}


\details{

  In this study, patients were randomly assigned to high-dose (750 mg
  per day), low-dose (375 mg per day), or placebo.  This dataset
  consists of a subset of data on patients who had floating gallstones
  and who were assigned to the high-dose and placebo groups.  In the
  NCGS it was suggested that chenodiol would dissolve gallstones but in
  doing so might increase levels of serum cholesterol.  As a result,
  serum cholesterol (mg/dL) was measured at baseline and at 6, 12, 20,
  and 24 months of follow-up.  Many cholesterol measurements are missing
  because of missed visits, laboratory specimens were lost or
  inadequate, or patient follow-up was terminated.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{cholesterol}.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Wei LJ, Lachin JM (1984) Two-Sample Asymptotically Distribution-Free
  Tests for Incomplete Multivariate Observations. Journal of the
  American Statistical Association 79:653-661.

}

% -------------------------------------------------------------------------

\examples{

data(cholesterol)
str(cholesterol)

}


\keyword{datasets}
