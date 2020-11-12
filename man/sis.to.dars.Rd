\name{sis.to.dars}
\alias{sis.to.dars}
\title{Convert SIS Term to DARS Term}
\description{
  Returns a vector of DARS Terms from a vector of SIS Terms.
}
\usage{
  dars.to.sis(SIS Term)
}
\arguments{
  \item{SIS Term}{Character vector containing SIS formatted Terms (eg. 1172)}
}
\details{
  This function returns a DARS formatted Term (eg. 20191, where term order is Fall, Spring, Summer) from a vector of SIS formatted Terms (eg. 1192).
}
\examples{
  sis.to.dars(c("1182", "1091", "1176", "0972"))
}