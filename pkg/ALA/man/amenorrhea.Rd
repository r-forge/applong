\name{amenorrhea}
\alias{amenorrhea}
\docType{data}

% -------------------------------------------------------------------------

\title{Amenorrhea from clinical trial of contracepting women}


\description{

  The data are from a longitudinal clinical trial of contracepting
  women.

}


\usage{data(amenorrhea)}


\format{

  A data frame with 4604 observations on the following 4 variables.

  \describe{
    
    \item{\code{id}}{a factor with 1151 levels}
    \item{\code{dose}}{a factor with levels \code{low} and \code{high}}
    \item{\code{day}}{a numeric vector; days since initial injection}
    \item{\code{amenorrhea}}{a factor with levels \code{no} and
      \code{yes}}
    
  }
  
}


\details{

  In this trial women received an injection of either 100 mg or
  150 mg of depot-medroxyprogesterone acetate (DMPA) on the day of
  randomization and three additional injections at 90-day intervals.
  There was a final follow-up visit 90 days after the fourth injection,
  i.e., one year after the first injection.  Throughout the study each
  woman completed a menstrual diary that recorded any vaginal bleeding
  pattern disturbances.  The diary data were used to determine whether a
  women experienced amenorrhea, the absence of menstrual bleeding for a
  specified number of days.  A total of 1151 women completed the
  menstrual diaries and the diary data were used to generate a binary
  sequence for each woman according to whether or not she had
  experienced amenorrhea in the four successive three month intervals.
  In clinical trials of modern hormonal contraceptives, pregnancy is
  exceedingly rare (and would be regarded as a failure of the
  contraceptive method), and is not the main outcome of interest in this
  study.  Instead, the outcome of interest is a binary response
  indicating whether a woman experienced amenorrhea in the four
  successive three month intervals.  A feature of this clinical trial is
  that there was substantial dropout.  More than one third of the women
  dropped out before the completion of the trial.

}


\note{

  Original variable names have been adapted to R conventions.  Variable
  day was recoded from the original sequential number to the
  corresponding number of days since initial injection.

}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}



\references{

  Machin D, Farley T, Busca B, Campbell M, d'Arcangues C (1988)
  Assessing changes in vaginal bleeding patterns in contracepting women.
  Contraception 38:165-179.

}

% -------------------------------------------------------------------------

\examples{

str(amenorrhea)
summary(amenorrhea)

}


\keyword{datasets}
