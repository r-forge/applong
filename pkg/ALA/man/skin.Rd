\name{skin}
\alias{skin}
\docType{data}

% -------------------------------------------------------------------------

\title{The skin cancer prevention study}


\description{

  The data are from the Skin Cancer Prevention Study, a randomized,
  double-blind, placebo-controlled clinical trial of beta-carotene to
  prevent non-melanoma skin cancer in high risk subjects.

}


\usage{data(skin)}

\format{
  
  A data frame with 7081 observations on the following 9 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 1683 levels}
    \item{\code{center}}{a factor with levels \code{C1}, \code{C2},
      \code{C3}, \code{C4}}
    \item{\code{age}}{a numeric vector}
    \item{\code{skin}}{a factor with levels \code{otherwise} and
      \code{burns}}
    \item{\code{gender}}{a factor with levels \code{F} and \code{M}}
    \item{\code{nExposure}}{a numeric vector; number of previous skin
      cancers}
    \item{\code{nCancers}}{a numeric vector; number of new skin cancers
      per year}
    \item{\code{treatment}}{a factor with levels \code{placebo} and
      \code{B-carotene}}
    \item{\code{year}}{a numeric vector; year of follow-up}
    
  }
  
}


\details{

  A total of 1805 subjects were randomized to either placebo or 50mg of
  beta-carotene per day for 5 years.  Subjects were examined once a year
  and biopsied if a cancer was suspected to determine the number of new
  skin cancers occurring since the last exam.  The outcome variable is a
  count of the number of new skin cancers per year.  The outcome
  variable Y is a count of the of the number of new skin cancers per
  year.  The categorical variable Treatment is coded 1=beta-carotene,
  0=placebo.  The variable Year denotes the year of follow-up. The
  categorical variable Gender is coded 1=male, 0=female.  The
  categorical variable Skin denotes skin type and is coded 1=burns,
  0=otherwise.  The variable Exposure is a count of the number of
  previous skin cancers.  The variable Age is the age (in years) of each
  subject at randomization.  Complete data are available on 1683
  subjects comprising a total of 7081 measurements.

}


\note{

  Original variable names have been adapted to R conventions.  The
  center ID was prefixed with the letter \dQuote{C} in the \code{center}
  variable, compared to the source.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Greenberg ER, Baron JA, Stukel TA, Stevens MM, Mandel JS, Spencer SK,
  Elias PM, Lowe N, Nierenberg DW, Bayrd G, Vance JC, Freeman DH,
  Clendenning WE, Kwan T, the Skin Cancer Prevention Study Group (1990)
  A clinical trial of beta carotene to prevent basal-cell and
  squamous-cell cancers of the skin. New England Journal of Medicine
  323:789-795

}

% -------------------------------------------------------------------------

\examples{

str(skin)
summary(skin)

}


\keyword{datasets}
