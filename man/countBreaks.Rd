% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conversion.R
\name{countBreaks}
\alias{countBreaks}
\title{Function to make custom categorical breaks}
\usage{
countBreaks(breaks, labels)
}
\arguments{
\item{breaks}{a sequence of values to use as break points for categorization.
The sequence corresponds to the left side of the bound. For example
0, 2 would represent counts cuts of 1 and 2+.}

\item{labels}{a sequence of character strings labelling the break points for categorization.
For example 1 and 2+ would be labels for 0, 2.}
}
\value{
Creates a breaksStrategy object holding the labels for categorization
}
\description{
Function to make custom categorical breaks
}
