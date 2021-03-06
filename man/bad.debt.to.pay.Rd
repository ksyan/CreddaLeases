% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LeaseFunctions.R
\name{bad.debt.to.pay}
\alias{bad.debt.to.pay}
\title{The bad.debt.to.pay function}
\usage{
bad.debt.to.pay(a.lease = test.average.lease, file = NULL, text.size = 12,
  small.text.size = 4)
}
\arguments{
\item{a.lease,}{average lease data}

\item{file,}{if this isn't null, the graph will be saved as this file}

\item{text.size,}{text size for graph titles and sides}

\item{small.text.size,}{text size for the smaller text inside the graph}
}
\value{
The graph of bad debt by month
}
\description{
Graphs bad debt received as a percentage of payment expected at that point
  on a by-month basis
}

