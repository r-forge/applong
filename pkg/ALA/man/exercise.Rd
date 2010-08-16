\name{exercise}
\alias{exercise}
\docType{data}

% -------------------------------------------------------------------------

\title{Subset of Data from Exercise Therapy Study}


\description{

  The data are from a study of exercise therapies, where 37 patients
  were assigned to one of two weightlifting programs

}


\usage{data(exercise)}


\format{
  
  A data frame with 259 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 37 levels}
    \item{\code{treatment}}{a factor with levels \code{repetitions} and
      \code{weights} representing whether repetitions or weights were
      increased, respectively}
    \item{\code{day}}{a numeric vector}
    \item{\code{strength}}{a numeric vector}
    
  }
  
}


\details{

  In the first program (treatment 1), the number of repetitions was
  increased as subjects became stronger.  In the second program
  (treatment 2), the number of repetitions was fixed but the amount of
  weight was increased as subjects became stronger.  Measures of
  strength were taken at baseline (day 0), and on days 2, 4, 6, 8, 10,
  and 12.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{strength}.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Freund RJ, Littell RC, Spector PC (1986) SAS Systems for Linear
  Models, Cary, NC: SAS Institute Inc.

}

% -------------------------------------------------------------------------

\examples{

str(exercise)
summary(exercise)

}


\keyword{datasets}
