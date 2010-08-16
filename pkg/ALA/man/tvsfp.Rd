\name{tvsfp}
\alias{tvsfp}
\docType{data}

% -------------------------------------------------------------------------

\title{Subset of data from the Television School and Family Smoking
  Prevention and Cessation Project (TVSFP)}


\description{

  The Television, School and Family Smoking Prevention and Cessation
  Project (TVSFP) was a study designed to determine the efficacy of a
  school-based smoking prevention curriculum in conjunction with a
  television-based prevention program, in terms of preventing smoking
  onset and increasing smoking cessation.

}


\usage{data(tvsfp)}

\format{
  
  A data frame with 3200 observations on the following 7 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 1600 levels}
    \item{\code{school}}{a factor with 28 levels}
    \item{\code{class}}{a factor with 135 levels}
    \item{\code{school.based}}{a factor with levels \code{no} and
      \code{yes}}
    \item{\code{tv.based}}{a factor with levels \code{no} and
      \code{yes}}
    \item{\code{stage}}{a factor with levels \code{pre} and \code{post},
      indicating whether observation is before or after randomization}
    \item{\code{THKS}}{a numeric vector}
    
  }
  
}


\details{

  The study used a 2 X 2 factorial design, with four intervention
  conditions determined by the cross-classification of a school-based
  social-resistance curriculum (CC: coded 1 = yes, 0 = no) with a
  television-based prevention program (TV: coded 1 = yes. 0 = no).
  Randomization to one of the four intervention conditions was at the
  school level, while much of the intervention was delivered at the
  classroom level.  The original study involved 6695 students in 47
  schools in Southern California.  This dataset consists of a subset of
  1600 seventh-grade students from 135 classes in 28 schools in Los
  Angeles.  The response variable, a tobacco and health knowledge scale
  (THKS), was administered before and after randomization of schools to
  one of the four intervention conditions.  The scale assessed a
  student's knowledge of tobacco and health.

}


\note{

  Original variable names have been adapted to R conventions.  Data were
  reshaped from wide to long format and the response named \code{THKS}.
  A new ID column was added, relative to the source.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  Flay BR, Miller TQ, Hedeker D, Siddiqui O, Brannon BR, Johnson CA,
  Hansen WB, Sussman S, Dent C (1995) The television, school and family
  smoking prevention and cessation project: VIII. Student outcomes and
  mediating variables.  Preventive Medicine 24:29-40

}

% -------------------------------------------------------------------------

\examples{

str(tvsfp)
summary(tvsfp)

}


\keyword{datasets}
