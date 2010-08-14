\name{headache}
\alias{headache}
\docType{data}

% -------------------------------------------------------------------------

\title{Data from randomized crossover study comparing two analgesic
  drugs (A and B) and placebo for relief of tension headaches}


\description{

  The study was designed to compare two active drugs and placebo for
  relief of tension headache.

}


\usage{data(headache)}


\format{
  
  A data frame with 846 observations on the following 6 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 423 levels}
    \item{\code{center}}{a factor with 14 levels}
    \item{\code{treatment.seq}}{a factor with levels \code{AB},
      \code{BA}, \code{AP}, \code{PA}, \code{BP}, \code{PB}}
    \item{\code{period}}{a factor with levels \code{0} and \code{1}}
    \item{\code{treatment}}{a factor with levels \code{A}, \code{B}, and
      \code{P}}
    \item{\code{relief}}{a numeric vector}
    
  }
  
}


\details{

  The two analgesic drugs were identical in their active ingredients
  except that one contained caffeine. The primary comparison of interest
  was between the two active drugs; the placebo was included for
  regulatory purposes.  Note that there were three treatments, but only
  two periods, i.e., each subject received only two of the three
  treatments in a random order.  With three treatments and two periods,
  there are six possible treatment sequences, AB, BA, AP, PA, BP, PB,
  where A, B and P denote the two active drugs and placebo.  In this
  study the AB and BA sequences were assigned three times as many
  subjects as the remaining four because of the interest in the A versus
  B comparison.

}


\note{

  Original variable names have been adapted to R conventions.  The third
  column in the source was excluded as it was a duplicate of the sixth.
  The center ID was prefixed with the letter \dQuote{C} in the
  \code{center} variable, compared to the source.

  Two headaches treated within each period and response is the average
  pain relief for both headaches.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Laird NM, Skinner J, Kenward M (1992) An analysis of two-period
  crossover designs with carry-over effects. Statistics in Medicine
  11:1967-1979

}

% -------------------------------------------------------------------------

\examples{

data(headache)
str(headache)

}


\keyword{datasets}
