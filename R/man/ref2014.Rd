\name{ref2014}
\alias{ref2014}
\docType{data}
\title{2014 Research Excellence Framework Results}
\description{
  This data set gives the Quality profiles for all REF2014 submissions.
}
\usage{data(ref2014)}
\format{A data frame with 2558 observations on the following 8
  variables:
  \describe{
    \item{\code{UKPRN}}{a factor representing the UK Provider Reference
    Number, allocated to the Institution by the UK Register of Learning
    Providers;}

    \item{\code{Institution}}{a character vector giving a human-readable
    form of the HE provider’s name;}

    \item{\code{UOA}}{a factor representing the REF 2014 Unit of
    Assessment;}

    \item{\code{UnitOfAssessment}}{a character vector giving a
    human-readable form of the REF 2014 Unit of Assessment;}

    \item{\code{msubId}}{a factor representing the multiple submission
    identifier for a submission to the same Unit of Assessment from the
    same HE provider, with levels \code{A}, \code{B}, \code{C} and
    \code{Not Recorded}; missing (\code{NA}) values indicate that there
    was only a single submission for that HE provider and Unit of
    Assessment;}

    \item{\code{StaffFte}}{a numeric vector representing the FTE
      Category A staff submitted.}
  }
}
\details{
  This data set was published by REF as the results of the Research
  Excellence Framework 2014.
}
\source{https://www.ref.ac.uk/}
\keyword{datasets}
