% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addChar.R
\name{addLabChar}
\alias{addLabChar}
\title{Add a lab characteristic}
\usage{
addLabChar(
  clinChar,
  labUnitTable,
  timeWindows,
  limit = c("last", "first", "all"),
  categorize = NULL
)
}
\arguments{
\item{clinChar}{a clinChar object maintaining the components of the characterization}

\item{labUnitTable}{a labUnitTable object specifying the lab-unit combos to search}

\item{timeWindows}{a timeWindow object that specifies the boundaries relative to the target start date
on when to search for the presence of a value. use \code{makeTimeTable} function}

\item{limit}{specify which values to use in the characteristic. The last variable will pull the last value in the
time window, the first variable will pull the first value in the time window and the
all vairable will pull all values in the time window}

\item{categorize}{describes how the continuous value should be categorized.
This function takes a breaksStrategy object to describe the categories ow it is left NULL.
If the parameter is NULL then no categorization summary is done}
}
\value{
adds a labChar object into the clinChar extractSettings slot
}
\description{
This function adds a lab characteristic to the clinChar object.
}
