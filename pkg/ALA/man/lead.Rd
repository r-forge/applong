\name{lead}
\alias{lead}
\docType{data}

% -------------------------------------------------------------------------

\title{Subsample (N=50) of data on Blood Lead Levels from the succimer
  arm of the Treatment of Lead Exposed Children (TLC) Trial}


\description{

  The Treatment of Lead-Exposed Children (TLC) trial was a
  placebo-controlled, randomized study of succimer (a chelating agent)
  in children with blood lead levels of 20-44 micrograms/dL.

}


\usage{data(lead)}


\format{

  A data frame with 200 observations on the following 3 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 50 levels}
    \item{\code{week}}{a numeric vector}
    \item{\code{lead}}{a numeric vector; lead level}
  }
}


\details{

  These data consist of four repeated measurements of blood lead levels
  obtained at baseline (or week 0), week 1, week 4, and week 6 on 50
  children who were randomly assigned to chelation treatment with
  succimer.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named \code{lead}.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Treatment of Lead-exposed Children (TLC) Trial Group (2000) Safety and
  Efficacy of Succimer in Toddlers with Blood Lead Levels of 20-44
  micrograms/dL.  Pediatric Research 48:593-599

}

% -------------------------------------------------------------------------

\examples{

str(lead)
summary(lead)

}


\keyword{datasets}
