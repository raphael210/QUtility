% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pub01_utilityFuncs.R
\name{ggplot.rtn.periods}
\alias{ggplot.rtn.periods}
\alias{rtn_periods_andrew}
\alias{rtn.periods2df}
\alias{rtn.rollperiods}
\title{ggplot.rtn.periods}
\usage{
\method{ggplot}{rtn.periods}(rtn, freq = "year", nwin = 3, roll = FALSE)

rtn.periods2df(rtn_result, melt = FALSE)

rtn.rollperiods(rtn, freq = c("year", "month", "quarter"), nwin = 3)
}
\arguments{
\item{nwin}{if roll is \code{TRUE}, nwin will be active.}

\item{roll}{default is \code{FALSE},if \code{TRUE}, param \code{freq} can only be one of \code{year},\code{month} and \code{quarter}.}
}
\description{
\code{ggplot.rtn.periods} plot rtn.periods result.

\code{rtn.periods2df} turn rtn.periods' result into data.frame.

\code{rtn.rollperiods} calculate rolling.periods' result into data.frame.
}
\seealso{
[QUtility::rtn.periods()].
}
