\name{cd4}
\alias{cd4}
\docType{data}

% -------------------------------------------------------------------------

\title{Repeated CD4 counts data from AIDS clinical trial}


\description{

  The data are from a randomized, double-blind, study of AIDS patients
  with advanced immune suppression (CD4 counts of less than or equal to
  50 cells/mm^3).

}


\usage{data(cd4)}


\format{

  A data frame with 5036 observations on the following 6 variables.
  
  \describe{

    \item{\code{id}}{a factor with 4992 levels}
    \item{\code{treatment}}{a factor with levels \code{zX400d}
      (zidovudine alternating monthly with 400 mg didanosine),
      \code{zA225z} (zidovudine plus 2.25 mg of zalcitabine),
      \code{zA400d} (zidovudine plus 400 mg of didanosine), and
      \code{zA400dA400n} (zidovudine plus 400 mg of didanosine plus 400
      mg of nevirapine).}
    \item{\code{age}}{a numeric vector}
    \item{\code{gender}}{a factor with levels \code{F} and \code{M}}
    \item{\code{week}}{a numeric vector; weeks since baseline time}
    \item{\code{logCD4}}{a numeric vector; log transformed
      CD4 counts (log(CD4 counts + 1))}

  }
  
}


\details{
  
  Patients in AIDS Clinical Trial Group (ACTG) Study 193A were
  randomized to dual or triple combinations of HIV-1 reverse
  transcriptase inhibitors.  Specifically, patients were randomized to
  one of four daily regimens containing 600mg of zidovudine: zidovudine
  alternating monthly with 400mg didanosine; zidovudine plus 2.25mg of
  zalcitabine; zidovudine plus 400mg of didanosine; or zidovudine plus
  400mg of didanosine plus 400mg of nevirapine (triple therapy).
  Measurements of CD4 counts were scheduled to be collected at baseline
  and at 8-week intervals during follow-up.  However, the CD4 count data
  are unbalanced due to mistimed measurements and missing data that
  resulted from skipped visits and dropout.  The number of measurements
  of CD4 counts during the first 40 weeks of follow-up varied from 1 to
  9, with a median of 4.  The response variable is the log transformed
  CD4 counts, log(CD4 counts + 1), available on 1309 patients.

}


\note{

  Original variable names have been adapted to R conventions.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}

\references{

  Henry K, Erice A, Tierney C, Balfour HH Jr, Fischl MA, Kmack A, Liou
  SH, Kenton A, Hirsch MS, Phair J, Martinez A, Kahn JO, for the AIDS
  Clinical Trial Group 193A Study Team (1998) A randomized, controlled,
  double-blind study comparing the survival benefit of four different
  reverse transcriptase inhibitor therapies (three-drug, two-drug, and
  alternating drug) for the treatment of advanced AIDS.  Journal of
  Acquired Immune Deficiency Syndromes and Human Retrovirology
  19:339-349.

}

% -------------------------------------------------------------------------

\examples{

str(cd4)
summary(cd4)

}


\keyword{datasets}
