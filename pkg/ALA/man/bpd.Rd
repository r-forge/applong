\name{bpd}
\alias{bpd}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on Bronchopulmonary Dysplasia (BPD) from low birth weight
  infants.}


\description{

  Data on Bronchopulmonary Dysplasia (BPD) from 223 low birth weight
  infants (weighing less than 1750 grams).  The data are from a study of
  low birth weight infants in a neonatal intensive care unit.

}


\usage{data(bpd)}


\format{

  A data frame with 223 observations on the following 5 variables.

  \describe{

    \item{\code{id}}{a factor with 223 levels}
    \item{\code{bpd}}{a numeric vector}
    \item{\code{birth.weight}}{a numeric vector}
    \item{\code{gestational.age}}{a numeric vector}
    \item{\code{toxemia}}{a numeric vector}

  }

}



\details{

  The study was designed to examine the development of bronchopulmonary
  dysplasia (BPD), a chronic lung disease, in a sample of 223 infants
  weighing less than 1750 grams.  The response variable is binary,
  denoting whether an infant develops BPD by day 28 of life (where BPD
  is defined by both oxygen requirement and compatible chest
  radiograph).

}


\note{

  Original variable names have been adapted to R conventions.  Variable
  ID was added to identify individuals.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Van Marter LJ, Leviton A, Kuban KCK, Pagano M, Allred EN (1990)
  Maternal glucocorticoid therapy and reduced risk of bronchopulmonary
  dysplasia.  Pediatrics 86:331-336.

}

% -------------------------------------------------------------------------

\examples{

data(bpd)
str(bpd)

}


\keyword{datasets}
