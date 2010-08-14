\name{toenail}
\alias{toenail}
\docType{data}

% -------------------------------------------------------------------------

\title{Multicenter study comparing two oral treatments for toe-nail
  infection}


\description{

  The data are from a randomized, double-blind, parallel-group,
  multicenter study comparing two oral treatments (denoted A and B) for
  toe-nail infection.

}


\usage{data(toenail)}


\format{
  
  A data frame with 1908 observations on the following 5 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 294 levels}
    \item{\code{onycholysis}}{a numeric vector (0: none or mild; 1:
      moderate or severe)}
    \item{\code{treatment}}{a factor with levels \code{A} and \code{B}}
    \item{\code{month}}{a numeric vector}
    \item{\code{week}}{a numeric vector}
    
  }
  
}


\details{

  Patients were evaluated for the degree of onycholysis (the degree of
  separation of the nail plate from the nail-bed) at baseline (week 0)
  and at weeks 4, 8, 12, 24, 36, and 48 thereafter.  The onycholysis
  outcome variable is binary (none or mild versus moderate or severe).
  The binary outcome was evaluated on 294 patients comprising a total of
  1908 measurements.

  The variable \code{month} denotes the exact timing of measurements in
  months.  The variable \code{week} denotes scheduled visits at 0, 4, 8,
  12, 24, 36, and 48 weeks.

}


\note{

  Original variable names have been adapted to R conventions.  Variable
  \code{visit} in the source was recoded to \code{week} to make its
  meaning more transparent.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  De Backer M, De Vroey C, Lesaffre E, Scheys I, De Keyser P (1998)
  Twelve weeks of continuous oral therapy for toenail onychomycosis
  caused by dermatophytes: A double-blind comparative trial of
  terbinafine 250 mg/day versus itraconazole 200 mg/day.  Journal of the
  American Academy of Dermatology 38:57-63

}

% -------------------------------------------------------------------------

\examples{

data(toenail)
str(toenail)

}


\keyword{datasets}
