\name{tumor}
\alias{tumor}
\docType{data}

% -------------------------------------------------------------------------

\title{Data on tumor recurrence for patients with bladder cancer}


\description{

  The data are from an experimental study of patients with bladder
  cancer conducted by the Veterans Administration Cooperative Urological
  Research Group.

}


\usage{data(tumor)}


\format{
  
  A data frame with 75 observations on the following 3 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 75 levels}
    \item{\code{treatment}}{a factor with levels \code{placebo} and
      \code{thiotepa}}
    \item{\code{recurrence}}{a numeric vector (0: no new tumors; 1: new
      tumors)}
    
  }
  
}


\details{

  In this study patients underwent surgery to remove tumors.  Following
  surgery, patients were randomized to either placebo or treatment with
  thiotepa.  Subsequently patients were examined at 18, 24, 30 and 36
  months.  The dataset contains information for the month 18 follow-up
  visit only.  The response variable is binary, indicating whether or
  not there is a new tumor (Y=1, if new tumor; Y=0, if no new tumor) at
  the 18 month visit.  The categorical variable \code{treatment} is
  coded 1=thiotepa, 0=placebo.

}


\note{

  Original variable names have been adapted to R conventions.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Wei LJ, Lin DY, Weissfeld L (1989) Regression analysis of multivariate
  incomplete failure time data by modeling marginal distribution.
  Journal of the American Statistical Association 84:1065-1071

}

% -------------------------------------------------------------------------

\examples{

data(tumor)
str(tumor)

}


\keyword{datasets}
