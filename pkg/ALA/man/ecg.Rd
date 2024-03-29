\name{ecg}
\alias{ecg}
\docType{data}

% -------------------------------------------------------------------------

\title{Two-period crossover trial study on cerebrovascular deficiency}


\description{

  The dataset consists of safety data from a crossover trial on the
  disease cerebrovascular deficiency.

}


\usage{data(ecg)}


\format{

  A data frame with 14 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 67 levels}
    \item{\code{sequence}}{a factor with levels \code{P->A} and
      \code{A->P}, indicating treatment sequence (placebo to active and
      active to placebo, respectively}
    \item{\code{period}}{a factor with levels \code{1} and \code{2}}
    \item{\code{ecg}}{a factor (ECG response) with levels
      \code{normal} and \code{abnormal}}
    
  }
  
}


\details{

  The response variable is not a trial endpoint but rather a potential
  side effect. In this two-period crossover trial, comparing the effects
  of active drug to placebo, 67 patients were randomly allocated to the
  two treatment sequences, with 34 patients receiving placebo followed
  by active treatment, and 33 patients receiving active treatment
  followed by placebo. The response variable is binary, indicating
  whether an electrocardiogram (ECG) was abnormal (1) or normal
  (0). Each patient has a bivariate binary response vector.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named \code{ecg}.
  Variable ID was added to identify individuals.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{
  
  Jones B, Kenward MG (1989) Design and Analysis of Cross-over Trials.
  London: Chapman and Hall/CRC Press.

}

% -------------------------------------------------------------------------

\examples{

str(ecg)
summary(ecg)

}


\keyword{datasets}
