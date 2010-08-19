\name{fat}
\alias{fat}
\docType{data}

% -------------------------------------------------------------------------

\title{Study of influence of menarche on changes in body fat accretion}


\description{

  The data are from a prospective study on body fat accretion in a
  cohort of 162 girls from the MIT Growth and Development Study. The
  study examined changes in percent body fat before and after
  menarche. The data represent a subset of the study materials and
  should not be used to draw substantive conclusions.

}


\usage{data(fat)}


\format{

  A data frame with 1049 observations on the following 5 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 162 levels}
    \item{\code{age}}{a numeric vector}
    \item{\code{age.menarche}}{a numeric vector; age at menarche}
    \item{\code{time.menarche}}{a numeric vector; time since menarche}
    \item{\code{percent.fat}}{a numeric vector}
  }
}


\details{

  At the start of the study, all of the girls were pre-menarcheal and
  non-obese, as determined by a triceps skinfold thickness less than the
  85th percentile.  All girls were followed over time according to a
  schedule of annual measurements until four years after menarche.  The
  final measurement was scheduled on the fourth anniversary of their
  reported date of menarche.  At each examination, a measure of body
  fatness was obtained based on bioelectric impedance analysis and a
  measure of percent body fat (\%BF) was derived.  In this data set
  there are a total of 1049 individual percent body fat measurements,
  with an average of 6.4 measurements per subject.  The numbers of
  measurements per subject pre- and post-menarche are approximately
  equal.

}


\note{

  Original variable names have been adapted to R conventions.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Phillips SM, Bandini LG, Compton DV, Naumova EN, Must A (2003) A
  longitudinal comparison of body composition by total body water and
  bioelectrical impedance in adolescent girls.  Journal of Nutrition
  133:1419-1425

}

% -------------------------------------------------------------------------

\examples{

str(fat)
summary(fat)

if (require(lattice)) {
    ## Fig. 8.5 (roughly)
    xyplot(percent.fat ~ time.menarche, data=fat, groups=id, type="b",
           cex=0.5, col=1,
           xlab="Time relative to menarche (weeks)",
           ylab="Percent body fat")
    ## Fig. 8.6 (roughly)
    xyplot(percent.fat ~ time.menarche, data=fat,
           cex=0.5, col=1,
           xlab="Time relative to menarche (years)",
           ylab="Percent body fat",
           panel=function(x, y, ...) {
               panel.abline(v=0, lty=2)
               panel.xyplot(x, y, ...)
               panel.loess(x, y, ...)
           })
}

if (require(lme4)) {
    fatNew <- within(fat, {
        stage <- pmax(time.menarche, 0)
    })
    summary(fatNew)
    ## Model in p. 218
    (fm1 <- lmer(percent.fat ~ time.menarche + stage + (time.menarche + stage | id),
                 data=fatNew))
    ## Table 8.7
    VarCorr(fm1)[[1]]
    ## Fig. 8.7 (roughly)
    set.seed(1234); rndID <- sample(levels(fatNew$id), 2)
    tm <- with(fatNew, seq(floor(min(time.menarche)),
                           ceiling(max(time.menarche))))
    fitted.pf <- fitted(fm1)
    avg.modmat <- cbind(1, tm, pmax(tm, 0))
    pred.fixef <- avg.modmat \%*\% fixef(fm1)
    plot(pred.fixef ~ avg.modmat[, 2], type="l", ylim=c(5, 35), lwd=2,
         xlab="Time relative to menarche (years)",
         ylab="Percent body fat")
    with(fatNew, {
        points(time.menarche[id == rndID[1]], percent.fat[id == rndID[1]])
        lines(time.menarche[id == rndID[1]], fitted.pf[id == rndID[1]])
        points(time.menarche[id == rndID[2]], percent.fat[id == rndID[2]], pch=2)
        lines(time.menarche[id == rndID[2]], fitted.pf[id == rndID[2]], lty=2)
    })
}

}


\keyword{datasets}
