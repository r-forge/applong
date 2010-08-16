\name{leprosy}
\alias{leprosy}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on count of leprosy bacilli pre- and post-treatment from a
  clinical trial of antibiotics for leprosy}


\description{

  The dataset consists of count data from a placebo-controlled clinical
  trial of 30 patients with leprosy at the Eversley Childs Sanitorium in
  the Philippines.

}


\usage{data(leprosy)}


\format{
  
  A data frame with 60 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 30 levels}
    \item{\code{drug}}{a factor with levels \code{A}, \code{B}, and
      \code{C} indicating drug treatment (C: placebo)}
    \item{\code{period}}{a factor with levels \code{pre} and
      \code{post}}
    \item{\code{nBacilli}}{a numeric vector; bacilli count}
  }
}


\details{

  Participants in the study were randomized to either of two antibiotics
  (denoted treatment drug A and B) or to a placebo (denoted treatment
  drug C).  Prior to receiving treatment, baseline data on the number of
  leprosy bacilli at six sites of the body where the bacilli tend to
  congregate were recorded for each patient.  After several months of
  treatment, the number of leprosy bacilli at six sites of the body were
  recorded a second time.  The outcome variable is the total count of
  the number of leprosy bacilli at the six sites.  In this study, the
  question of main scientific interest is whether treatment with
  antibiotics (drugs A and B) reduces the abundance of leprosy bacilli
  at the six sites of the body when compared to placebo (drug C).

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{nBacilli}.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Snedecor GW, Cochran WG  (1967)  Statistical Methods, (6th
  edn).  Ames, Iowa: Iowa State University Press

}

% -------------------------------------------------------------------------

\examples{

str(leprosy)
summary(leprosy)

}


\keyword{datasets}
