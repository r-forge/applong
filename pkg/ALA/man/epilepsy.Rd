\name{epilepsy}
\alias{epilepsy}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on seizure counts for 59 epileptics}


\description{

  The data are from a placebo-controlled clinical trial of 59
  epileptics.  Patients with partial seizures were enrolled in a
  randomized clinical trial of the anti-epileptic drug, progabide.

}


\usage{data(epilepsy)}


\format{

  A data frame with 295 observations on the following 5 variables.
  
  \describe{

    \item{\code{id}}{a factor with 59 levels}
    \item{\code{treatment}}{a factor with levels \code{placebo} and
      \code{progabide}}
    \item{\code{age}}{a numeric vector}
    \item{\code{week}}{a numeric vector; 0 represents the baseline over
      the preceeding 8 weeks}
    \item{\code{nSeizures}}{a numeric vector; number of seizures}

  }
  
}


\details{

  Participants in the study were randomized to either progabide or a
  placebo, as an adjuvant to the standard anti-epileptic
  chemotherapy. Progabide is an anti-epileptic drug whose primary
  mechanism of action is to enhance gamma-aminobutyric acid (GABA)
  content; GABA is the primary inhibitory neurotransmitter in the brain.
  Prior to receiving treatment, baseline data on the number of epileptic
  seizures during the preceding 8-week interval were recorded.  Counts
  of epileptic seizures during 2-week intervals before each of four
  successive post-randomization clinic visits were recorded.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{nSeizures}.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Thall PF, Vail SC (1990) Some covariance models for longitudinal count
  data with overdispersion. Biometrics 46:657-671

}

% -------------------------------------------------------------------------

\examples{

data(epilepsy)
str(epilepsy)

}


\keyword{datasets}
