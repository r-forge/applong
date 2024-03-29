\name{tlc}
\alias{tlc}
\docType{data}

% -------------------------------------------------------------------------

\title{Subsample (N=100) of data on blood lead levels from the Treatment
  of Lead Exposed Children (TLC) trial}


\description{

  The Treatment of Lead-Exposed Children (TLC) trial was a
  placebo-controlled, randomized study of succimer (a chelating agent)
  in children with blood lead levels of 20-44 micrograms/dL.

}


\usage{data(tlc)}


\format{
  
  A data frame with 400 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 100 levels}
    \item{\code{treatment}}{a factor with levels \code{placebo} and
      \code{succimer}}
    \item{\code{week}}{a numeric vector}
    \item{\code{lead}}{a numeric vector}
    
  }
  
}


\details{

  These data consist of four repeated measurements of blood lead levels
  obtained at baseline (or week 0), week 1, week 4, and week 6 on 100
  children who were randomly assigned to chelation treatment with
  succimer or placebo.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named
  \code{lead}.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Treatment of Lead-exposed Children (TLC) Trial Group. (2000).  Safety
  and Efficacy of Succimer in Toddlers with Blood Lead Levels of 20-44
  micrograms/dL.  Pediatric Research 48:593-599

}

% -------------------------------------------------------------------------

\examples{

str(tlc)
summary(tlc)

if (require(lattice)) {
    ## Fig. 6.5 (roughly)
    xyplot(lead ~ week, data=tlc, groups=treatment, type=c("p", "a"),
           cex=0.5,
           xlab="Time (weeks)", ylab="Mean blood lead level (mcg/dL)",
           scales=list(rot=c(0, 1), tck=c(0.5, 0)))
}

if (require(lme4)) {
    tlcNew <- within(tlc, {
        week1 <- ifelse(week > 1, week - 1, 0)
    })
    summary(tlcNew)
    ## Assuming this is the random effect structure used in p. 155
    lmer(lead ~ week + week1 + week:treatment + week1:treatment +
         (week + week1 | id), data=tlcNew)
}

}


\keyword{datasets}
