\name{ALA-package}
\alias{ALA-package}
\alias{ALA}
\docType{package}

% -------------------------------------------------------------------------

\title{Data sets and examples for the book "Applied Longitudinal Analysis"}


\description{

  Data sets used in Fitzmaurice et al.'s book and models fit therein
  using lme4.

}


\details{

  The package provides all the data sets from Fitzmaurice et al.'s book,
  and fits some of the models therein using lme4.

  The following data sets are not included, since they are already
  available elsewhere:

  \describe{
    
    \item{dental:}{\code{data(Orthodont, package="nlme")}}
    \item{melanoma:}{\code{data(Mmmec, package="mlmRev")}}
    
  }

}


\references{

  Fitzmaurice GM, Laird NM, and Ware JH (2004) Applied Longitudinal
  Analysis. Wiley.

}


\seealso{

  \code{\link[lme4]{lmer}}

}

% -------------------------------------------------------------------------

\author{

  Sebastian P. Luque and Douglas Bates

  Maintainer: Sebastian P. Luque \email{spluque@gmail.com}

}


\keyword{ package }
