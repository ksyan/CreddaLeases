% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LeaseFunctions.R
\name{cash.by.day}
\alias{cash.by.day}
\title{The cash.by.day function}
\usage{
cash.by.day(a.lease = test.average.lease, file = NULL, text.size = 12,
  small.text.size = 4, assumptions = default.constants.list)
}
\arguments{
\item{a.lease,}{average lease data}

\item{file,}{if this isn't null, the graph will be saved as this file}

\item{text.size,}{text size for graph titles and sides}

\item{small.text.size,}{text size for the smaller text inside the graph}

\item{assumptions,}{a list of constant values}
}
\value{
The graph of cumulative cash received per lease by day
}
\description{
Graphs cumulative cash received per lease by day
}

