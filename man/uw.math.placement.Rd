\name{uw.math.placement}
\alias{uw.math.placement}
\title{Find the UW-Madison Math Placement from Scores}
\description{
  Returns a vector of either placement levels or placement text.  This will use either the current algorithm or the pre-2017 algorithm by setting MFND_COMPONENT_ID.
}
\usage{
  uw.math.placement(MFND, AALG, TAG, MFND_COMPONENT_ID = "MFND",  
                              output_type = "numeric")
}
\arguments{
  \item{MFND}{Numeric Foundation score}
  
  \item{AALG}{Numeric Algebra score}
  
  \item{TAG}{Numeric Trigonometry score}
  
  \item{MFND_COMPONENT_ID}{The TEST_COMPONENT_ID for the Foundation score.  Is a character string.  This sets the algorithm used for placement.  This should be "MFND" (default) for the current algorithm or "MBSC" for the pre-2017 algorithm.}
  
  \item{output_type}{The type of output, either "numeric" or "text".  This defaults to numeric.}
}
\details{
  This function returns either a numeric representation or text of UW-Madison Placement scores.  For the numeric represenation, it returns 1-8 (lower number means lower placement) that refers to the boxes in the grid of the UW-Madison Math Placement Algorithm.  The text output produces long strings that suck to use, so it is not recommended.  It is provided primarily for reference.
}
\examples{
  uw.math.placement(c(450, 670, 555), c(598, 469, 344), c(722, 358, 658))
  uw.math.placement(MFND = c(450, 670, 555), AALG = c(598, 469, 344), TAG = c(722, 358, 658), MFND_COMPONENT_ID = c("MFND", "MBSC", "MBSC"))
}