\name{hesa2014}
\alias{hesa2014}
\docType{data}
\title{HESA REF Contextual data}
\description{
  This data set gives the scaled FTE of staff eligible to be submitted
  to the REF 2014 by HE provider and REF Unit of Assessment.
}
\usage{data(hesa2014)}
\format{A data frame with 2558 observations on the following 8
  variables:
  \describe{
    \item{\code{INSTID}}{a factor representing the HESA Institution
    Identifier;}

    \item{\code{UKPRN}}{a factor representing the UK Provider Reference
    Number, allocated to the Institution by the UK Register of Learning
    Providers;}

    \item{\code{Region}}{a factor representing the region of the UK
    associated with the administrative centre of the Institution, from:
    \code{NEAS} (North East), \code{NWES} (North West), \code{YORH}
    (Yorkshire and The Humber), \code{EMID} (East Midlands), \code{WMID}
    (West Midlands), \code{EAST} (East of England), \code{LOND}
    (London), \code{SEAS} (South East), \code{SWES} (South West),
    \code{SCOT} (Scotland), \code{WALE} (Wales), and \code{NIRE}
    (Northern Ireland);}

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

    \item{\code{EligibleFte}}{a numeric vector representing the Scaled
    Eligible FTE totals for the submission, according to HESA’s
    records.}
  }
}
\details{
  This data set was published by HESA to give additional context to the
  results of the REF 2014, published by HEFCE.  For each HE provider
  (University), Unit of Assessment (subject area), and multiple
  submission identifier, this data set gives the “Scaled FTE” of staff
  eligible to be submitted to the REF, according to HESA’s records.

  This version of the data set contains the estimates as amended by
  subsequent error identification by Queen Mary University of London and
  by Cardiff University.  It also recodes the Institute of Education as
  the “A” submission for the Education Unit of Assessment (25) for
  University College London, following the latter’s absorption of the
  former and to reflect the coding for the corresponding submissions in
  the \code{\link{ref2014}} dataset.

  Note that for several HE providers there are staff in HESA’s records
  whose corresponding Unit of Assessment is not recorded.  Attempts to
  join this dataset with \code{\link{ref2014}} will need some care.
}
\source{https://www.hesa.ac.uk/ref2014}
\keyword{datasets}