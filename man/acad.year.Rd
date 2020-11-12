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