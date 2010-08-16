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

}


\keyword{datasets}
