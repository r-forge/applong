\name{smoking}
\alias{smoking}
\docType{data}

% -------------------------------------------------------------------------

\title{Subsample (N=133) of data on FEV1 from the Vlagtwedde-Vlaardingen
  study}


\description{

  In an epidemiologic study conducted in two different areas in the
  Netherlands, the rural area of Vlagtwedde in the north-east and the
  urban, industrial area of Vlaardingen in the south-west, residents
  were followed over time to obtain information on the prevalence of and
  risk factors for chronic obstructive lung diseases.

}


\usage{data(smoking)}


\format{
  
  A data frame with 771 observations on the following 4 variables.
  
  \describe{
    
    \item{\code{id}}{a factor with 133 levels}
    \item{\code{smoker}}{a factor with levels \code{former} and
      \code{current}}
    \item{\code{year}}{a numeric vector; years since baseline
      measurement}
    \item{\code{FEV1}}{a numeric vector; forced expiratory volume}
    
  }
  
}


\details{

  This dataset is based on the sample of men and women from the rural
  area of Vlagtwedde. The sample, initially aged 15-44, participated in
  follow-up surveys approximately every 3 years for up to 21 years.  At
  each survey, information on respiratory symptoms and smoking status
  was collected by questionnaire and spirometry was performed.  Pulmonary
  function was determined by spirometry and a measure of forced
  expiratory volume (FEV1) was obtained every three years for the first
  15 years of the study, and also at year 19.

  The dataset is comprised of a sub-sample of 133 residents aged 36 or
  older at their entry into the study and whose smoking status did not
  change over the 19 years of follow-up.  Each study participant was
  either a current or former smoker. Current smoking was defined as
  smoking at least one cigarette per day.  In this dataset FEV1 was not
  recorded for every subject at each of the planned measurement
  occasions.  The number of repeated measurements of FEV1 on each
  subject varied from 1 to 7.

}


\note{

  Original variable names have been adapted to R conventions.
  
}


\source{

  \url{http://biosun1.harvard.edu/~fitzmaur/ala}

}


\references{

  van der Lende R, Kok TJ, Peset R, Quanjer PH, Schouten JP, Orie NGM
  (1981) Decreases in VC and FEV1 with time: Indicators for effects of
  smoking and air pollution. Bulletin of European Physiopathology and
  Respiration 17:775-792

}

% -------------------------------------------------------------------------

\examples{

str(smoking)
summary(smoking)

if (require(lme4)) {
    ## Assuming this is the random effect structure in p. 152
    lmer(FEV1 ~ smoker * year + (year | id), data=smoking,
         control=list(maxIter=2000))
}

}


\keyword{datasets}
