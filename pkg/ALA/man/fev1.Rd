\name{fev1}
\alias{fev1}
\docType{data}

% -------------------------------------------------------------------------

\title{Subsample (N=300) of data on FEV1 and height for girls living in
  Topeka from the Six Cities Study of Air Pollution and Health}


\description{

  The Six Cities Study of Air Pollution and Health was a longitudinal
  study designed to characterize lung growth as measured by changes in
  pulmonary function in children and adolescents, and the factors that
  influence lung function growth.

}


\usage{data(fev1)}


\format{

  A data frame with 1994 observations on the following 6 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 300 levels}
    \item{\code{age}}{a numeric vector}
    \item{\code{height}}{a numeric vector}
    \item{\code{age0}}{a numeric vector; initial age}
    \item{\code{height0}}{a numeric vector; initial height}
    \item{\code{logFEV1}}{a numeric vector}
  }
}


\details{

  A cohort of 13,379 children born on or after 1967 was enrolled in six
  communities across the U.S.: Watertown (Massachusetts), Kingston and
  Harriman (Tennessee), a section of St. Louis (Missouri), Steubenville
  (Ohio), Portage (Wisconsin), and Topeka (Kansas).  Most children were
  enrolled in the first or second grade (between the ages of six and
  seven) and measurements of study participants were obtained annually
  until graduation from high school or loss to follow-up. At each annual
  examination, spirometry, the measurement of pulmonary function, was
  performed and a respiratory health questionnaire was completed by a
  parent or guardian.

  The dataset contains a subset of the pulmonary function data collected
  in the Six Cities Study. The data consist of all measurements of FEV1,
  height and age obtained from a randomly selected subset of the female
  participants living in Topeka, Kansas. The random sample consists of
  300 girls, with a minimum of one and a maximum of twelve observations
  over time.

}


\note{

  Original variable names have been adapted to R conventions.  \code{age0} and
  \code{height0} are just the first element of the \code{age} and
  \code{height} variables for each subject.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Dockery DW, Berkey CS, Ware JH, Speizer FE, Ferris BG (1983)
  Distribution of FVC and FEV1 in children 6 to 11 years old. American
  Review of Respiratory Disease 128:405-412

}

% -------------------------------------------------------------------------

\examples{

str(fev1)
summary(fev1)

if (require(lattice)) {
    fev1.e <- exp(fev1$logFEV1)
    set.seed(1234); ids <- sample(levels(fev1$id), 50)
    bwplot(~ logFEV1, data=fev1)        # reveals the outlier mentioned in text
    subset(fev1, logFEV1 < -0.5)
    ## Fig. 8.4 (roughly)
    xyplot(log(fev1.e/height) ~ age, data=fev1, groups=id, type="b",
           subset=id \%in\% ids, cex=0.5, col=1,
           xlab="Age (years)", ylab="Log(FEV1/Height)",
           ylim=c(-0.3, 1.2))
}

if (require(lme4)) {
    ## Model in p. 213
    (fm1 <- lmer(logFEV1 ~ age + log(height) + age0 + log(height0) + (age | id),
                 data=fev1, subset=logFEV1 > -0.5))
    ## Table 8.3
    VarCorr(fm1)$id * 100

    ## Model in p. 216
    (fm2 <- update(fm1, . ~ . - (age | id) + (log(height) | id)))
}


}


\keyword{datasets}
