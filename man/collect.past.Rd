% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/LeaseFunctions.R
\name{collect.past}
\alias{collect.past}
\title{The collect.past function}
\usage{
\method{collect}{past}(leases = default.past.leases,
  assumptions = default.constants.list)
}
\arguments{
\item{assumptions,}{list of constant values}

\item{dataframe}{with data about leases. Needs to have fields for lease status, pay frequency, price,
down payment, start date,installments, installment dates, installment balances, installment pay dates,
installment states, payments, and payment dates.}
}
\value{
A by-day dataframe with financial parameters payment.due, payment.expected, payment.process.cost,
  asset.acq.outflow, total.assets.linear, total.assets.loan, lease.depreciation, bad.debt
}
\description{
This function takes in a dataframe (default.past.leases) containing data about leases,
  and returns a dataframe with financial parameters
}

