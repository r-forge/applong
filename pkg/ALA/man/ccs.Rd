\name{ccs}
\alias{ccs}
\docType{data}

% -------------------------------------------------------------------------

\title{Connecticut Child Surveys}


\description{

  The data are from two surveys of children's mental health. A
  standardized measure of childhood psychopathology was used both by
  parents (Child Behavior Checklist, CBC) and teachers (Teacher Report
  Forms, TRF) to assess children in the study

}


\usage{data(ccs)}


\format{

  A data frame with 2501 observations on the following 5 variables.

  \describe{
    \item{\code{id}}{a factor with 2501 levels}
    \item{\code{problems}}{a factor with levels \code{good} and \code{bad}}
    \item{\code{parent.status}}{a factor with levels \code{otherwise}
      and \code{single}}
    \item{\code{parent.report}}{a numeric vector}
    \item{\code{teacher.report}}{a numeric vector}
  }

}


\details{

  The response variable is derived from the externalizing scale, which
  assesses delinquent and aggressive behavior.  The scale has been
  dichotomized at the cut point for borderline/clinical psychopathology.

  Because of the multiple levels of permissions and reporting, a
  substantial number of children were missing the TRF.  The data set
  consists of 1428 children who had both parent and teacher responses,
  and an additional 1073 children with only a parent response; a total
  of 2501 children participated in the study.  The two sources or
  respondents are the children's parents and teachers; in the
  psychiatric literature, these sources are often referred to as
  \dQuote{informants}.  Explanatory variables include single parent
  status and child's physical health problems.

}


\note{

  Original variable names have been adapted to R conventions.  Factor
  problems level \code{bad} corresponds to \dQuote{fair to bad} in
  Fitzmaurice's et al. source.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Zahner GEP, Jacobs JH, Freeman DH and Trainor KF (1993) Rural-urban
  child psychopathology in a northeastern U.S. state: 1986--1989.
  Journal of the American Academy of Child and Adolescent Psychiatry
  32:378-387.

}

% -------------------------------------------------------------------------

\examples{

data(ccs)
str(ccs)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{datasets}
