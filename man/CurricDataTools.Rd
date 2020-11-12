\name{acad.year}
\alias{acad.year}
\title{Convert Term to Academic Year}
\description{
Returns a vector of Academic Years from a vector of either SIS Terms or DARS Terms.
}
\usage{
acad.year(Term, TermType)
}
\arguments{
  \item{Term}{Character vector containing SIS or DARS terms}

  \item{TermType}{The type of terms input: "SIS" or "DARS"}
}
\details{
This function returns the Academic Year (term order is Summer, Fall, Spring) from either SIS formatted terms or DARS formatted terms.  This is formatted as the full year, without the Semester.  For instance, 2019.
}
\examples{
acad.year(c("1182", "1094", "1176", "0972"), "SIS")
}




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



