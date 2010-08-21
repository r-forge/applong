\name{cd4}
\alias{cd4}
\docType{data}

% -------------------------------------------------------------------------

\title{Repeated CD4 counts data from AIDS clinical trial}


\description{

  The data are from a randomized, double-blind, study of AIDS patients
  with advanced immune suppression (CD4 counts of less than or equal to
  50 cells/mm^3).

}


\usage{data(cd4)}


\format{

  A data frame with 5036 observations on the following 6 variables.
  
  \describe{

    \item{\code{id}}{a factor with 4992 levels}
    \item{\code{treatment}}{a factor with levels \code{zX400d}
      (zidovudine alternating monthly with 400 mg didanosine),
      \code{zA225z} (zidovudine plus 2.25 mg of zalcitabine),
      \code{zA400d} (zidovudine plus 400 mg of didanosine), and
      \code{zA400dA400n} (zidovudine plus 400 mg of didanosine plus 400
      mg of nevirapine).}
    \item{\code{age}}{a numeric vector}
    \item{\code{gender}}{a factor with levels \code{F} and \code{M}}
    \item{\code{week}}{a numeric vector; weeks since baseline time}
    \item{\code{logCD4}}{a numeric vector; log transformed
      CD4 counts (log(CD4 counts + 1))}

  }
  
}


\details{
  
  Patients in AIDS Clinical Trial Group (ACTG) Study 193A were
  randomized to dual or triple combinations of HIV-1 reverse
  transcriptase inhibitors.  Specifically, patients were randomized to
  one of four daily regimens containing 600mg of zidovudine: zidovudine
  alternating monthly with 400mg didanosine; zidovudine plus 2.25mg of
  zalcitabine; zidovudine plus 400mg of didanosine; or zidovudine plus
  400mg of didanosine plus 400mg of nevirapine (triple therapy).
  Measurements of CD4 counts were scheduled to be collected at baseline
  and at 8-week intervals during follow-up.  However, the CD4 count data
  are unbalanced due to mistimed measurements and missing data that
  resulted from skipped visits and dropout.  The number of measurements
  of CD4 counts during the first 40 weeks of follow-up varied from 1 to
  9, with a median of 4.  The response variable is the log transformed
  CD4 counts, log(CD4 counts + 1), available on 1309 patients.

}


\note{

  Original variable names have been adapted to R conventions.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}

\references{

  Henry K, Erice A, Tierney C, Balfour HH Jr, Fischl MA, Kmack A, Liou
  SH, Kenton A, Hirsch MS, Phair J, Martinez A, Kahn JO, for the AIDS
  Clinical Trial Group 193A Study Team (1998) A randomized, controlled,
  double-blind study comparing the survival benefit of four different
  reverse transcriptase inhibitor therapies (three-drug, two-drug, and
  alternating drug) for the treatment of advanced AIDS.  Journal of
  Acquired Immune Deficiency Syndromes and Human Retrovirology
  19:339-349.

}

% -------------------------------------------------------------------------

\examples{

str(cd4)
summary(cd4)

if (require(lattice)) {
    ## Create new data with just 2 levels of treatment
    cd4New <- within(cd4, {
        treatment <- factor(ifelse(treatment != levels(treatment)[4], 0, 1),
                            labels=c("double", "triple"))
    })
    ## Fig. 8.9 (roughly)
    xyplot(logCD4 ~ week, data=cd4New, groups=treatment, type="smooth",
           aspect=1.5, cex=0.5, col=1, lty=c(2, 1),
           scales=list(rot=c(0, 1), tck=c(0.5, 0)), ylim=c(2.5, 3.5),
           xlab="Time (weeks)", ylab="Log(CD4 + 1)")
}

if (require(lme4)) {
    str(cd4New)
    cd4New <- within(cd4New, {
        ## Use a stage factor -- this is what is needed to make the same
        ## interpretations as in the book
        stage <- cut(week, breaks=c(floor(min(week)), 16,
                             ceiling(max(week))),
                     labels=c("pre", "post"), include.lowest=TRUE)
        ## But this is what is actually used
        stage.tij <- ifelse(week > 16, week - 16, 0)
    })
    summary(cd4New)
    ## Model in p. 227
    (fm1 <- lmer(logCD4 ~ week + stage.tij + week:treatment +
                 stage.tij:treatment + (week + stage.tij | id), data=cd4New))
\dontrun{
    ## This should be an equivalent model using the stage factor, but we
    ## don't get quite the same coefficients
    (fm1b <- lmer(logCD4 ~ week + week:stage + week:treatment +
                  week:stage:treatment + (week + week:stage | id), data=cd4New,
                  control=list(maxIter=2000, maxFN=2000)))
}
    ## Table 8.13
    VarCorr(fm1)[[1]] * 1000
    ## Model in p. 229 -- we don't get quite the same coefficients
    (fm2 <- lmer(logCD4 ~ week + stage.tij + treatment:(week - stage) +
                 age + gender + (week + stage.tij | id), data=cd4New))

    ## Fig. 8.7 (roughly)
    set.seed(12)
    rndID <- as.character(with(cd4New,
                               sample(unique(id[treatment == "triple" &
                                                gender == "M"]), 2)))
    wk <- with(cd4New, seq(floor(min(week)), ceiling(max(week))))
    st <- ifelse(wk > 16, wk - 16, 0)
    mm <- model.matrix(logCD4 ~ week + stage + treatment:(week - stage) +
                       age + gender,
                       data=with(cd4New,
                         data.frame(treatment=treatment[treatment == "triple"][1],
                                    age=45, gender=gender[gender == "M"][1],
                                    week=wk, stage=st, logCD4=rnorm(length(wk)))))
    pred.fixef <- mm \%*\% fixef(fm2)
    fitted.cd4 <- fitted(fm2)
    plot(pred.fixef ~ mm[, 2], type="l", lwd=2, ylim=range(fitted.cd4),
         xlab="Time (weeks)",
         ylab="Log(CD4 + 1)")
    with(cd4New, {
        points(week[id == rndID[1]], logCD4[id == rndID[1]])
        lines(week[id == rndID[1]], fitted.cd4[id == rndID[1]])
        points(week[id == rndID[2]], logCD4[id == rndID[2]], pch=2)
        lines(week[id == rndID[2]], fitted.cd4[id == rndID[2]], lty=2)
    })
}

}


\keyword{datasets}
