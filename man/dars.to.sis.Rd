\name{dars.to.sis}
\alias{dars.to.sis}
\title{Convert DARS Term to SIS Term}
\description{
  Returns a vector of SIS Terms from a vector of DARS Terms.
}
\usage{
  dars.to.sis(DARS Term)
}
\arguments{
  \item{DARS Term}{Character vector containing DARS formatted Terms (eg. 20171)}
}
\details{
  This function returns a SIS formatted Term (eg. 1172, where term order is Fall, Spring, Summer) from a vector of DARS formatted Terms (eg. 20171).
}
\examples{
  dars.to.sis(c("20181", "20092", "20173", "19971"))
}