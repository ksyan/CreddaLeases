#  load("data/default_constants_list.Rda")
#  load("data/default_constants_table.Rda")
#  load("data/default_employees.Rda")
#  load("data/default_end_month.Rda")
#  load("data/default_growth_rates.Rda")
#  load("data/default_int_lev.Rda")
#  load("data/default_lease_att.Rda")
#  load("data/default_past_leases.Rda")
#  load("data/default_start_month.Rda")
#  load("data/norm_default.Rda")
#  load("data/norm_early_default.Rda")
 library(dplyr)
 library(ggplot2)
 library(xlsx)
#
#' The reverse.irr function
#'
#' This function calculates the irr of a lease (as if it was a loan) given an
#' initial value and a payment schedule, used here as a helper for asset.val
#' @param init.val is the starting value of the lease, or the amount of money lent
#' @param pay schedule is a vector of payments in the lease, make sure it has no gaps in the schedule
#' @return a number representing the IRR of the inputs
#' @export
#' @examples
#' reverse.irr(init.val = 1200, pay.schedule = rep(200,10))
reverse.irr <- function(init.val,
                        pay.schedule){
  f <- function (y){
    sum(pay.schedule / (1 + y)^(1: length(pay.schedule))) - init.val
  }
  uniroot(f, c(-1,1))$root
}

#' The asset.val function
#'
#' This function calculates the asset value of a lease given an initial value and a payment schedule
#' @param init.val is the starting value of the lease, or the amount of money lent
#' @param pay schedule is a vector of payments in the lease
#' @return a vector representing the value of the asset over the pay.schedule
#' @export
#' @examples
#' asset.val(init.val = 1200, pay.schedule = c(200,0,0,200,0,200,0,200,0,200,200,0,0,200,200,0,200,0,200))
asset.val <- function(init.val,
                      pay.schedule){
  relevant.pay.indexes <- c(which(pay.schedule != 0))
  relevant.pay <- pay.schedule[relevant.pay.indexes]

  temp = rep(init.val, length(relevant.pay))
  toReturn <- rep(0,length(pay.schedule))

  temp[1] = init.val - (relevant.pay[1] - reverse.irr(init.val = init.val, pay.schedule = relevant.pay) * init.val)

  for (x in 2:length(relevant.pay)){
    temp[x] = temp[x-1] -
      (relevant.pay[x] - reverse.irr(init.val = init.val, pay.schedule = relevant.pay) * temp[x-1])
    toReturn[relevant.pay.indexes[x-1] : (relevant.pay.indexes[x] - 1)] <- temp[x-1]
  }
  toReturn[1: relevant.pay.indexes[1] - 1] <- init.val
  toReturn[which(toReturn < 0)] <- 0

  toReturn
}

#' The lease.combinations function
#'
#' This function calculates the all possible combinations of leases given a set of attributes
#' @param lease.att, a list of lists that represent different distributions of lease traits
#' @return a data frame where each row represents a lease
#' @export
lease.combinations <- function(lease.att = default.lease.att) {

  combs <- expand.grid(lease.length = lease.att$lease.length$param.types,
                       markup = lease.att$markup$param.types,
                       sales.price = lease.att$sales.price$param.types,
                       init.pay.pct = lease.att$init.pay.pct$param.types,
                       pay.freq = lease.att$pay.freq$param.types,
                       pay.manner = lease.att$pay.manner$param.types)
  calc.weights <- function(a.row){
    lease.att$lease.length$param.dist[which(lease.att$lease.length$param.types == as.numeric(a.row['lease.length']))] *
      lease.att$markup$param.dist[which(lease.att$markup$param.types == as.numeric(a.row['markup']))]*
      lease.att$sales.price$param.dist[which(lease.att$sales.price$param.types == as.numeric(a.row['sales.price']))] *
      lease.att$init.pay.pct$param.dist[which(lease.att$init.pay.pct$param.types == as.numeric(a.row['init.pay.pct']))] *
      lease.att$pay.freq$param.dist[which(lease.att$pay.freq$param.types == a.row['pay.freq'])] *
      lease.att$pay.manner$param.dist[which(lease.att$pay.manner$param.types == a.row['pay.manner'])]
  }
  combs$weights = apply(combs, 1, calc.weights)
  combs
}

test.lease.combinations <- lease.combinations()

#' The prd function
#'
#' This function calculates a predicted payments vector given data about previous payments and installments
#' @param co.indicator, a boolean indicating whether the lease is charged off.
#'    If charged off, predicted payments vector is just 0s.
#' @param past.payments, A vector containing past payments.
#' @param past.pay.dates, A vector containing the dates of past payments (corresponds 1:1 with past.payments)
#' @param installments, A vector with all the installments of the lease
#' @param installment.states, A vector containing the states of all installments (corresponds 1:1 with installments)
#' @param installments.balance, A vector with the balance remaining of all installments. Is 0 if the installment is
#'    paid. (corresponds 1:1 with installments)
#' @param installment.dates, A vector containing the dates each installment is due (corresponds 1:1 with installments)
#' @param installment.pay.dates, A vector containing the dates paid installments were paid. Corresponds 1:1 with installments,
#'    but is NA for installments that have not been paid.
#' @param installment.indexes, A vector containing the indexes of each installment in the larger data frame. (corresponds
#'    1:1 with installments)
#' @param pay.gap, An integer representing the number of days between scheduled payments
#' @param start.index, An integer representing the index matching this lease's start date in the larger data frame
#' @param start.date, The start date of the lease
#' @param paid.buckets, Vector of integers representing different "buckets" of ranges for leases that have already been partially
#'    paid. Should be same lenghth as paid.bucket.vals
#' @param paid.bucket.vals, Vector of values that correspond to each bucket in paid.buckets. Index 1 in paid.bucket.vals
#'    corresponds with values under index 1 in paid.buckets, Index 2 in paid.bucket.vals corresponds with values between
#'    indexes 1 and 2 in paid.buckets, and so on.
#' @param new.buckets, Vector of integers representing different "buckets" of ranges for brand new lesases. Should be same length as
#'    new.bucket.vals
#' @param new.bucket.vals: Vector of values that correspond to each bucket in new.buckets.Indexes correspond the same way as paid.bucket.vals
#'    and paid.buckets.
#' @param assumptions: constant values, represented as a list
#' @param default.rate: default rate represented as a vector 1000 long with values between 0 and 1
#' @return A vector representing the predicted payments for the future.
#' @export

prd <- function(co.indicator = FALSE, past.payments, past.pay.dates, installments, installment.states,
                installments.balance, installment.dates, installment.pay.dates, installment.indexes,
                pay.gap, start.index, start.date, paid.buckets = c(0,5,20,40,1000),
                paid.bucket.vals = c(.1,.5,1,2,4), new.buckets = c(0,30,60,1000), new.bucket.vals = c(.5,.5,.75,1),
                assumptions = default.constants.list, default.rate = norm.default.rate){
  if (!co.indicator){
    # Leases only have predicted future payments if they aren't charged off.
    paid.indexes = which(installment.states == "paid")
    unpaid.indexes = which(installment.states == "unpaid")

    if (length(past.payments) != 0){
      # If there have been past payments that data is taken into account when predicting future payments

      if (length(paid.indexes) == 0){
        # If no installments have been fully paid yet
        future.payments <- installments.balance[unpaid.indexes]
        average.days.late = sum(past.pay.dates - installment.dates[1])
      } else {
        # If installments have already been paid in the past
        last.paid = (which(installment.states == "unpaid")[1] - 1)
        average.prop.paid = sum(past.payments[which(past.pay.dates <= installment.pay.dates[last.paid])]) /
          sum(installments[1:last.paid])
        average.days.late = round(mean(installment.pay.dates[paid.indexes] - installment.dates[paid.indexes]))

        if (average.prop.paid > 1){
          # If people "overpay" on installments, they get a discount and potentially less payments total
          future.payments = sum(installments.balance[unpaid.indexes]) - cumsum(installments.balance[unpaid.indexes])
          + ((average.prop.paid-1) * installments.balance[unpaid.indexes] / (1 - assumptions$early.pay.discount))
          future.payments[which(future.payments < 0)] <- 0
          future.payments <- pmin(future.payments,average.prop.paid * installments.balance[unpaid.indexes])
        } else {
          future.payments <- installments.balance[unpaid.indexes]
        }
      }
      # Depending on how many days late payments are, the default rate will be amplified to a certain extent
      default.amp = paid.bucket.vals[min(which(paid.buckets > average.days.late))]
      future.payments <- future.payments *
        cumprod(default.rate[1 + installment.indexes[unpaid.indexes] - start.index]^(pay.gap * 12 / 365 * default.amp))
      return(future.payments)
    } else if (length(past.payments) == 0){
      # If there have been no past payments
      average.days.late = (Sys.Date() - start.date)
      # Depending on how late the first payment has been outstanding, default rate will be amplified
      default.amp = new.bucket.vals[min(which(new.buckets > average.days.late))]
      future.payments <- installments.balance[unpaid.indexes] *
        cumprod(default.rate[1 + installment.indexes - start.index]^(pay.gap * 12 / 365 * default.amp))
      return(future.payments)
    }
  } else {
    # If lease is charged off, predicted payments are just 0s
    return(rep(0, length(which(installment.states == "unpaid"))))
  }
}

#' The collect.past function
#'
#' This function takes in a dataframe (default.past.leases) containing data about leases,
#'   and returns a dataframe with financial parameters
#'
#' @param dataframe with data about leases. Needs to have fields for lease status, pay frequency, price,
#'   down payment, start date,installments, installment dates, installment balances, installment pay dates,
#'   installment states, payments, and payment dates.
#' @param assumptions, list of constant values
#' @return A by-day dataframe with financial parameters payment.due, payment.expected, payment.process.cost,
#'   asset.acq.outflow, total.assets.linear, total.assets.loan, lease.depreciation, bad.debt
#' @export
collect.past <- function(leases = default.past.leases,
                         assumptions = default.constants.list){
  toReturn <- data.frame(dates = seq(min(leases$start.date) - 10, by = "day",
                                     length.out = (max(leases$start.date) - min(leases$start.date) + 1012)),
                         payment.due = 0,
                         payment.expected = 0,
                         payment.process.cost = 0,
                         asset.acq.outflow = 0,
                         total.assets.linear = 0,
                         total.assets.loan = 0,
                         lease.depreciation = 0,
                         bad.debt = 0,
                         leases.booked = 0,
                         ongoing.leases = 0)
  # Iterate through each lease
  for (x in 1: nrow(leases)) {
    payments.vector = rep(0,length(toReturn$payment.expected))
    pdue.vector = rep(0,length(toReturn$payment.due))

    if (leases$status[x] == "returned"){
      # If leases are "returned" they basically don't exist except for payments arleady made

      past.payment.indexes <- unlist(sapply(leases$payment.dates[[x]], function(date){which(date == toReturn$dates)}))

      # Group the past payments so that payments on same days are combined
      unique.pay.indexes = unique(past.payment.indexes)
      unique.payments = rep(0, length(unique.pay.indexes))

      for (i in 1:length(past.payment.indexes)){
        unique.payments[which(unique.pay.indexes == past.payment.indexes[i])] =
          unique.payments[which(unique.pay.indexes == past.payment.indexes[i])] +
          leases$lease.payments[[x]][i]
      }

      # Past payments
      toReturn$payment.expected[unique.pay.indexes] = toReturn$payment.expected[unique.pay.indexes] + unique.payments
    } else {
      start.index <- which(toReturn$dates == leases$start.date[x])

      toReturn$leases.booked[start.index] <- toReturn$leases.booked[start.index] + 1

      # Payment gap
      pay.gap = if (leases$freq[[x]] == "weekly") 7
      else if (leases$freq[[x]] == "biweekly") 14
      else if (leases$freq[[x]] == "monthly") 365/12
      else if (substr(leases$freq[[x]],1,11) == "semimonthly") 15

      # Add down payment in payment vectors
      toReturn$payment.due[start.index] <- toReturn$payment.due[start.index] + leases$down.payments[x]
      toReturn$payment.expected[start.index] <- toReturn$payment.expected[start.index] + leases$down.payments[x]
      pdue.vector[start.index] <- pdue.vector[start.index] + leases$down.payments[x]
      payments.vector[start.index] <- payments.vector[start.index] +leases$down.payments[x]

      # Indexes of installments in toReturn
      installment.indexes <- unlist(sapply(leases$installment.dates[[x]], function(date){which(date == toReturn$dates)}))

      # Payments Due
      toReturn$payment.due[installment.indexes] = toReturn$payment.due[installment.indexes] + leases$installments[[x]]
      pdue.vector[installment.indexes] = pdue.vector[installment.indexes] + leases$installments[[x]]

      # Indexes of payments in toReturn
      past.payment.indexes <- unlist(sapply(leases$payment.dates[[x]], function(date){which(date == toReturn$dates)}))

      # Group the past payments so that payments on same days are combined
      unique.pay.indexes = unique(past.payment.indexes)
      unique.payments = rep(0, length(unique.pay.indexes))

      for (i in 1:length(past.payment.indexes)){
        unique.payments[which(unique.pay.indexes == past.payment.indexes[i])] =
          unique.payments[which(unique.pay.indexes == past.payment.indexes[i])] +
          leases$lease.payments[[x]][i]
      }

      # Past payments
      toReturn$payment.expected[unique.pay.indexes] = toReturn$payment.expected[unique.pay.indexes] + unique.payments
      payments.vector[unique.pay.indexes] = payments.vector[unique.pay.indexes] + unique.payments

      # Asset.Acq.Outflow
      toReturn$asset.acq.outflow[start.index] <- toReturn$asset.acq.outflow[start.index] + leases$price[x] + leases$down.payments[x]

      # Linear Asset Valuation

      # How long a lease lasts
      lease.length.days <- if (leases$status[[x]] == "paid") (as.numeric(max(leases$payment.dates[[x]]) - leases$start.date[x]) + 1)
      else (as.numeric(max(leases$installment.dates[[x]]) - leases$start.date[x]) + 1)

      # How long after the start.date until first payment
      first.pay.gap <- default.past.leases$installment.dates[[x]][1] - default.past.leases$start.date[x]
      if (first.pay.gap < 0) first.pay.gap = 0

      if (lease.length.days < 0){
        # If lease is paid off before its start.date it has no value as an asset
        temp.linear = 0
      } else if (lease.length.days < first.pay.gap) {
        # If the lease is paid off before its first installment is due value goes to 0 from that point on
        coeff <- (leases$price[x] + leases$down.payments[x])  / lease.length.days
        temp.linear = rep(0, length(toReturn$total.assets.linear))
        temp.linear[(start.index + 1): (start.index + lease.length.days + 1)] = leases$price[x] + leases$down.payments[x]
        toReturn$ongoing.leases[start.index : (start.index + lease.length.days)] =
          toReturn$ongoing.leases[start.index : (start.index + lease.length.days)] + 1
      } else {
        # Else, the lease value is a flat line until the first installment is due, when it depreciates linearly
        coeff <- (leases$price[x] + leases$down.payments[x])  / lease.length.days
        temp.linear = rep(0, length(toReturn$total.assets.linear))
        temp.linear[(start.index + 1): (start.index + first.pay.gap + 1)] = leases$price[x] + leases$down.payments[x]
        temp.linear[(start.index + first.pay.gap + 2): (start.index + lease.length.days)] =
          leases$price[x] + leases$down.payments[x] - coeff*c(1:(lease.length.days - first.pay.gap - 1))
        toReturn$ongoing.leases[start.index : (start.index + lease.length.days)] =
          toReturn$ongoing.leases[start.index : (start.index + lease.length.days)] + 1
      }

      # Loan assets valuation

      if (lease.length.days < 30){
        # If lease length is small, just make it the same as the linear valuation
        temp.loan = temp.linear
      } else {
        # If lease length is large, use asset.val to generate an asset curve
        temp.loan <- if (leases$status[[x]] == "paid") (asset.val(init.val = leases$price[x] + leases$down.payments[x], pay.schedule = payments.vector))
        else (asset.val(init.val = leases$price[x] + leases$down.payments[x], pay.schedule = pdue.vector))
        temp.loan[1:start.index] <- 0
      }

      # Predicting payments
      if (leases$status[[x]] == "active"){
        # Only active payments will have future payments
        unpaid.indexes = which(default.past.leases$installment.states[[x]] == "unpaid")

        if ((Sys.Date() - leases$installment.dates[[x]][unpaid.indexes[1]]) > assumptions$charge.off.time && length(leases$lease.payments[[x]]) != 0){
          # If the lease hasn't had any new payments for a certain time, it's charged off and the asset value/bad debt is changed to reflect that
          temp.linear[which(toReturn$dates == leases$installment.dates[[x]][unpaid.indexes[1]] + assumptions$charge.off.time) : length(temp.linear)] <- 0
          bad.debt.temp = leases$installments.balance[[x]][unpaid.indexes]
          toReturn$bad.debt[installment.indexes[unpaid.indexes]] = toReturn$bad.debt[installment.indexes[unpaid.indexes]] + bad.debt.temp
        } else if ((Sys.Date() - leases$start.date[x]) > assumptions$charge.off.time && length(leases$lease.payments[[x]]) == 0) {
          # If the lease hasn't had any payments at all for a certain time, it's charged off and the asset value/bad debt is changed
          temp.linear <- rep(0, length(temp.linear))
          bad.debt.temp = leases$installments.balance[[x]]
          toReturn$bad.debt[installment.indexes] = toReturn$bad.debt[installment.indexes] + bad.debt.temp
        } else {
          # Else, use prd to predict payments and add those payments to the appropriate vectors
          toAdd <-   prd(past.payments = leases$lease.payments[[x]], past.pay.dates = leases$payment.dates[[x]],
                         installments = leases$installments[[x]], installment.states = leases$installment.states[[x]],
                         installments.balance = leases$installments.balance[[x]], installment.dates = leases$installment.dates[[x]],
                         installment.pay.dates = leases$installment.pay.dates[[x]], installment.indexes = installment.indexes,
                         pay.gap = pay.gap, start.index = start.index, start.date = leases$start.date[x])
          toReturn$payment.expected[installment.indexes[unpaid.indexes]] =
            toReturn$payment.expected[installment.indexes[unpaid.indexes]] + toAdd

          payments.vector[installment.indexes[unpaid.indexes]] =
            payments.vector[installment.indexes[unpaid.indexes]] + toAdd

          bad.debt.toAdd = leases$installments.balance[[x]][unpaid.indexes] - toAdd
          bad.debt.toAdd[which(bad.debt.toAdd < 0)] = 0
          toReturn$bad.debt[installment.indexes[unpaid.indexes]] = toReturn$bad.debt[installment.indexes[unpaid.indexes]] + bad.debt.toAdd
        }
      }

      # Now, update the toReturn linear and loan assets
      toReturn$total.assets.linear <- toReturn$total.assets.linear + temp.linear
      toReturn$total.assets.loan = toReturn$total.assets.loan + temp.loan

      # Lease Depreciation
      pay.indexes <- which(payments.vector != 0)
      pay.indexes <- pay.indexes[which(pay.indexes > start.index)]
      if (length(pay.indexes) == 0){
        # If there are no payments after the start date, the lease just completely depreciates at that date
        pay.indexes = start.index + 1
        deprec.values = temp.linear[start.index + 1]
      } else if (length(pay.indexes) == 1) {
        # If there is only 1 payment after the start date, the lease completely depreciates at that payment
        deprec.values = temp.linear[start.index + 1]
      } else{
        # If there are multiple payments, use an algorithm to calculate depreciation values between payments
        deprec.values <- 1:length(pay.indexes)
        deprec.values[1] = temp.linear[start.index + 1] - temp.linear[pay.indexes[1]]
        for (i in 2: length(pay.indexes)){
          deprec.values[i] = temp.linear[pay.indexes[i - 1]] - temp.linear[pay.indexes[i]]
        }
        if (temp.linear[pay.indexes[length(pay.indexes)]] != 0){
          deprec.values[length(pay.indexes)] = deprec.values[length(pay.indexes)] + temp.linear[pay.indexes[length(pay.indexes)]]
        }
      }

      # Check if any payments are NA, if so just set to 0.
      if (any(is.na(deprec.values))){
        deprec.values = 0
      }

      toReturn$lease.depreciation[pay.indexes] = toReturn$lease.depreciation[pay.indexes] + deprec.values
    }
  }

  # Payment.process.cost is just a percentage of the payment.expected
  toReturn$payment.process.cost = toReturn$payment.expected * assumptions$pay.process.pct

  toReturn
}

test.collect.past <- collect.past()

#' The calc.average function
#'
#' Calculates an average lease given a large distribution of leases
#'
#' @param leases, a data.frame where each row is a lease
#' @param assumptions, list of constant values
#' @param default.rate, a vector representing the default rate, must be 1000 length with values between 0 and 1
#' @param early.default.rate, a vector representing the early default rate, must be 1000 length with values between 0 and 1
#' @return a data frame representing cash flow metrics of the average of input leases
#' @export
calc.average <- function(leases = test.lease.combinations,
                          assumptions =  default.constants.list,
                          default.rate = norm.default.rate,
                          early.default.rate = norm.early.default.rate){

  # Initialize the data.frame, set days to 2 years
  toReturn <- data.frame(days = c(1:1000),
                         payment.due = 0,
                         payment.expected = 0,
                         payment.process.cost = 0,
                         asset.acq.outflow = 0,
                         total.assets.linear = 0,
                         total.assets.loan = 0,
                         lease.depreciation = 0,
                         bad.debt = 0,
                         payments.sum = 0)
  for (x in 1: nrow(leases)) {
    # multiply sales price by weight
    this.sales.price <- leases[x, 'sales.price']*leases[x, 'weights']

    # time between each payment depends on lease type
    payment.gap <- if (leases[x, 'pay.freq'] == "w") 7
    else if (leases[x, 'pay.freq'] == "b") 14
    else if (leases[x, 'pay.freq'] == "s") floor(365/24)
    else if (leases[x, 'pay.freq'] == "m") floor(365/12)

    # the number of payments if this was a normal lease
    no.payments.norm <- if (leases[x, 'pay.freq'] == "w") c(1:floor(leases[x, 'lease.length'] * 365 / 84))
    else if (leases[x, 'pay.freq'] == "b") c(1:floor(leases[x, 'lease.length'] * 365 / 168))
    else if (leases[x, 'pay.freq'] == "s") c(1: (leases[x, 'lease.length'] * 2))
    else if (leases[x, 'pay.freq'] == "m") c(1: leases[x, 'lease.length'])

    # First values representing the initial payment,same for pay due, pay expected, and asset acquisition cost
    initial.pay <- this.sales.price * leases[x, 'init.pay.pct']
    toReturn$payment.due[1] = toReturn$payment.due[1] + initial.pay
    toReturn$payment.expected[1] = toReturn$payment.expected[1] + initial.pay
    toReturn$asset.acq.outflow[1] = toReturn$asset.acq.outflow[1] + initial.pay

    # payment due is calculated as if lease was normal
    payment.amt <- (this.sales.price - initial.pay / (1 + assumptions$sales.tax)) * leases[x, 'markup'] / (1-assumptions$sales.tax)/ length(no.payments.norm)
    toReturn$payment.due[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] <-
      toReturn$payment.due[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] + payment.amt

    # Asset acquisition outflow is also always the same
    toReturn$asset.acq.outflow[assumptions$delivery.time] <-
      toReturn$asset.acq.outflow[assumptions$delivery.time] + assumptions$discount*(this.sales.price - initial.pay)

    # payment expected (and processing cost) depends on type of lease
    if (leases[x, 'pay.manner']  == "90D"){
      no.payments.actual <-
        if (leases[x, 'pay.freq'] == "w") c(1: floor((90 + assumptions$delivery.time - assumptions$first.pay.gap)/7))
      else if (leases[x, 'pay.freq'] == "b") c(1:floor((90 + assumptions$delivery.time - assumptions$first.pay.gap)/14))
      else if (leases[x, 'pay.freq'] == "s") c(1: floor((90 + assumptions$delivery.time - assumptions$first.pay.gap) / 365 * 24))
      else if (leases[x, 'pay.freq'] == "m") c(1: floor((90 + assumptions$delivery.time - assumptions$first.pay.gap) / 365 * 12))

      payment.amt.actual <- (this.sales.price - initial.pay / (1 + assumptions$sales.tax)) * assumptions$ninetyD.markup / (1-assumptions$sales.tax)

      toReturn$payment.expected[assumptions$delivery.time + assumptions$ninetyD.gap] <-
        toReturn$payment.expected[assumptions$delivery.time + assumptions$ninetyD.gap] + payment.amt.actual

      toReturn$payment.process.cost[assumptions$delivery.time + assumptions$ninetyD.gap] <-
        toReturn$payment.process.cost[assumptions$delivery.time + assumptions$ninetyD.gap] + payment.amt.actual * assumptions$pay.process.pct

      coeff <- ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)  / (length(no.payments.norm) * (payment.gap - 1) + 1 - assumptions$first.pay.gap)
      temp.depr <- rep(0,1000)
      temp.depr[1:(assumptions$first.pay.gap - 1)] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)
      temp.depr[assumptions$first.pay.gap : 1000] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay) - coeff*c(1:(1000 - assumptions$first.pay.gap + 1))
      temp.depr[(assumptions$first.pay.gap + assumptions$ninetyD.gap) : 1000] <- 0

      toReturn$total.assets.linear <- toReturn$total.assets.linear + temp.depr
      toReturn$total.assets.loan <- toReturn$total.assets.loan + temp.depr

      toReturn$lease.depreciation[assumptions$delivery.time + assumptions$ninetyD.gap] =
        toReturn$lease.depreciation[assumptions$delivery.time + assumptions$ninetyD.gap] + temp.depr[1]
    }
    else if (leases[x, 'pay.manner'] == "early"){

      # the amount they actually pay, should be more than the supposed amount
      actual.pay <- payment.amt * (1 + assumptions$early.pay.amt)

      # the amount their payment actually counts towards their balance
      effective.pay <- payment.amt+ payment.amt*assumptions$early.pay.amt/(1 - assumptions$early.pay.discount)

      # the last payment is different because it's whatever's left of their balance they owe
      no.payments.actual <- c(1:(1+((length(no.payments.norm) * payment.amt) %/% effective.pay)))
      last.payment <- ((this.sales.price - initial.pay) * leases[x, 'markup'] / (1-assumptions$sales.tax))%%effective.pay

      # temporary vector to hold expected payments
      temp <- rep(0,length(toReturn$payment.expected))
      temp[((no.payments.actual-1) * payment.gap) + assumptions$first.pay.gap] <- actual.pay
      temp[max(which(temp != 0))] <- last.payment

      amt.financed <- (this.sales.price - initial.pay) * assumptions$discount + initial.pay

      tempdue <- rep(0,1000)
      tempdue[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] <- payment.amt

      temp2 <- asset.val(init.val = amt.financed, pay.schedule = tempdue)

      temp2[(assumptions$first.pay.gap + (length(no.payments.actual) * payment.gap)) : 1000] <- 0
      toReturn$total.assets.loan = toReturn$total.assets.loan + temp2

      # account for default rate
      temp[((no.payments.actual-1) * payment.gap) + assumptions$first.pay.gap] <-
        temp[((no.payments.actual-1) * payment.gap) + assumptions$first.pay.gap] *
        cumprod(early.default.rate[(no.payments.actual -1) * payment.gap + 1]^(payment.gap/(365/12)))

      bad.debt <- rep(0, length(toReturn$payment.expected))
      bad.debt[which(temp != 0)] <- actual.pay - temp[which(temp!= 0)]
      bad.debt[max(which(temp != 0))] <- last.payment - temp[max(which(temp != 0))]
      toReturn$bad.debt <- toReturn$bad.debt + bad.debt

      toReturn$payment.expected <- toReturn$payment.expected + temp
      toReturn$payment.process.cost <- toReturn$payment.process.cost + temp*assumptions$pay.process.pct

      coeff <- ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)  / (length(no.payments.norm) * (payment.gap) - assumptions$first.pay.gap)
      temp.depr <- rep(0,1000)
      temp.depr[1:(assumptions$first.pay.gap - 1)] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)
      temp.depr[assumptions$first.pay.gap : 1000] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay) - coeff*c(1:(1000 - assumptions$first.pay.gap + 1))
      temp.depr[(assumptions$first.pay.gap + (length(no.payments.actual) * payment.gap)) : 1000] <- 0
      toReturn$total.assets.linear <- toReturn$total.assets.linear + temp.depr

      pay.indexes <- which(temp != 0)
      deprec.values <- 1:length(pay.indexes)
      deprec.values[1] = temp.depr[1] - temp.depr[pay.indexes[1]]
      for (i in 2: length(pay.indexes)){
        deprec.values[i] = temp.depr[pay.indexes[i - 1]] - temp.depr[pay.indexes[i]]
      }
      toReturn$lease.depreciation[which(temp != 0)] = toReturn$lease.depreciation[which(temp != 0)] + deprec.values
    }
    else {
      # temporary vector to hold expected payments
      temp <- rep(0,length(toReturn$payment.expected))
      temp[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] <- payment.amt
      amt.financed <- (this.sales.price - initial.pay) * assumptions$discount + initial.pay

      tempdue <- rep(0,1000)
      tempdue[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] <- payment.amt
      temp2 <- asset.val(init.val = amt.financed, pay.schedule = tempdue)
      toReturn$total.assets.loan = toReturn$total.assets.loan + temp2

      # account for default rate
      temp[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] <-
        temp[((no.payments.norm-1) * payment.gap) + assumptions$first.pay.gap] *
        cumprod(default.rate[(no.payments.norm-1) * payment.gap + 1]^(payment.gap/(365/12)))

      bad.debt <- rep(0, length(toReturn$payment.expected))
      bad.debt[which(temp != 0)] <- payment.amt - temp[which(temp!= 0)]
      toReturn$bad.debt <- toReturn$bad.debt + bad.debt

      toReturn$payment.expected <- toReturn$payment.expected + temp
      toReturn$payment.process.cost <- toReturn$payment.process.cost + temp*assumptions$pay.process.pct
      coeff <- ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)  / (length(no.payments.norm) * (payment.gap - 1) + 1 - assumptions$first.pay.gap)
      temp.depr <- rep(0,1000)
      temp.depr[1:(assumptions$first.pay.gap - 1)] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay)
      temp.depr[assumptions$first.pay.gap : 1000] = ((this.sales.price - initial.pay) * assumptions$discount + initial.pay) - coeff*c(1:(1000 - assumptions$first.pay.gap + 1))

      temp.depr[which(temp.depr < 0)] <- 0
      toReturn$total.assets.linear <- toReturn$total.assets.linear + temp.depr

      pay.indexes <- which(temp != 0)
      deprec.values <- 1:length(pay.indexes)
      deprec.values[1] = temp.depr[1] - temp.depr[pay.indexes[1]]
      for (i in 2: length(pay.indexes)){
        deprec.values[i] = temp.depr[pay.indexes[i - 1]] - temp.depr[pay.indexes[i]]
      }
      toReturn$lease.depreciation[which(temp != 0)] = toReturn$lease.depreciation[which(temp != 0)] + deprec.values
    }
  }

  toReturn$payments.sum <- (sum(toReturn$payment.expected) - cumsum(toReturn$payment.expected)) *
    sum(toReturn$payment.due) / sum(toReturn$payment.expected)

  toReturn
}

test.average.lease <- calc.average()

#' The projections.by.day function
#'
#' This function projects future lease volume given a start date, end date, and projections
#'
#' @param start.date, the first date for projections to be shown
#' @param end date, the last date for projections to be shown
#' @param growth.projections, a dataframe with dates, rates, and a "per what"
#' @return a projections for each day
#' @export
projections.by.day <- function(start.date = head(as.Date(default.growth.rates$Dates, "%m/%d/%Y"), 1),
                               end.date= tail(as.Date(default.growth.rates$Dates, "%m/%d/%Y"), 1),
                               growth.projections = data.frame(date = as.Date(as.vector(default.growth.rates$Dates), "%m/%d/%Y"),
                                                               rate = as.vector(default.growth.rates$Leases),
                                                               per.what = 28)) {
  # check start.date and end.date attributes
  if((class(start.date) %in% "Date") == FALSE) stop ("start.date must be an object of class Date")
  if((class(end.date) %in% "Date") == FALSE) stop ("end.date must be an object of class Date")
  if((length(start.date) == 1) == FALSE) stop ("start.date must contain 1 date")
  if((length(end.date) == 1) == FALSE) stop ("end.date must contain 1 date")

  start.date.out <- min(c(start.date,growth.projections$date))
  end.date.out <- max(c(end.date,growth.projections$date))
  # create a data frame to hold the daily volume projections
  len <- as.numeric(end.date.out - start.date.out + 1)
  out <- data.frame(date=seq(start.date.out, by=1, len=len),
                    booked.unrounded=0,
                    booked=0)

  # order the growth projections in case not ordered
  growth.projections <- growth.projections[order(growth.projections$date),]

  # check there are enough growth projections for algorithm to run
  ngp <- nrow(growth.projections)
  if(ngp < 1) stop("At least 1 date needs to be provided in growth.projections")

  # before the earliest and after the last growth projections, the rate of growth is constant
  out$booked.unrounded[which(out$date <= growth.projections$date[1])] <- growth.projections$rate[1]/growth.projections$per.what[1]
  out$booked.unrounded[which(out$date >= growth.projections$date[ngp])] <- growth.projections$rate[ngp]/growth.projections$per.what[1]

  if(ngp > 1) {
    for(j in 2:ngp) {
      len <- as.numeric(growth.projections$date[j] - growth.projections$date[(j-1)])
      weights <- c(0:len) / len
      out$booked.unrounded[which(out$date >= growth.projections$date[(j-1)] &
                                   out$date <= growth.projections$date[j])] <- 1/growth.projections$per.what[1] *
        ((1-weights)*growth.projections$rate[(j-1)] + (weights)*growth.projections$rate[j])
    }
  }

  # calculate the estimated booked deals on each day
  rcm <- round(cumsum(out$booked.unrounded))
  out$booked <- rcm - c(0,rcm[1:(length(rcm)-1)])

  # only allow outputted dates within the start and end dates
  out <- out[which(out$date >= start.date & out$date <= end.date),c("date","booked")]

  return(out)
}

test.projections.by.day = projections.by.day()

#' The cash.flow function
#'
#' This function calculates cash flow metrics over a period of time given growth projections,
#'   average lease data, and historical lease data.
#'
#' @param hist.leases, data frame with information of historical leases
#' @param average.lease, data on cash metrics of the average lease
#' @param lease.growth, growth projections for future leases
#' @param assumptions, a list of constant values
#' @param emp.sheet, information about employees
#' @return a data frame with aggregate cash flow metrics over a period of time
#' @export
cash.flow <- function(hist.leases = test.collect.past,
                      average.lease = test.average.lease,
                      lease.growth =  test.projections.by.day,
                      assumptions = default.constants.list,
                      emp.sheet = default.employees){
  # Initialize the data.frame, set f
  toReturn <- data.frame()
  # Find the first day of the month of the earliest lease, this is the date where data.frame will start
  first.of.month <- as.Date(paste(substr(hist.leases$dates[1],1,8),"01",sep=""))

  toReturn <- data.frame(dates = seq(first.of.month, by="day",length.out = (max(lease.growth$date) -
                                                                              hist.leases$dates[1] + 1000)))

  # Initialize all the parameters of the data frame, most are 0 except costs that are constant across days
  toReturn$apps.per.month <- 0
  toReturn$average.app.per.day <- 0
  toReturn$average.app.per.day.diff <- 0
  toReturn$leases.booked <- 0
  toReturn$ongoing.leases <- 0
  toReturn$payment.due <- 0
  toReturn$payment.expected <- 0
  toReturn$asset.acq.outflow <- 0
  toReturn$total.assets.linear <- 0
  toReturn$total.assets.loan <- 0
  toReturn$lease.depreciation <- 0
  toReturn$payment.process.cost <- 0
  toReturn$no.employees <- 0
  toReturn$no.csm <- 0
  toReturn$no.salesperson <- 0
  toReturn$total.salaries <- 0
  toReturn$other.salaries <- 0
  toReturn$sales.salaries <- 0
  toReturn$csm.salaries <- 0
  toReturn$employee.tax <- 0
  toReturn$employee.benefit <- 0
  toReturn$rent.cost <- 0
  toReturn$third.pt.data.cost <- 0
  toReturn$it.cost <- floor(assumptions$it.base * 12 / 365 * 100)/100
  toReturn$legal.cost <- floor(assumptions$legal.base.month * 12 / 365 * 100) / 100
  toReturn$ops.cost <- 0
  toReturn$payments.sum <- 0

  past.indexes <- (1 + hist.leases$dates[1] - toReturn$dates[1]) : (nrow(hist.leases) + hist.leases$dates[1] - toReturn$dates[1])
  toReturn$payment.due[past.indexes] = toReturn$payment.due[past.indexes] + hist.leases$payment.due
  toReturn$payment.expected[past.indexes] = toReturn$payment.expected[past.indexes] + hist.leases$payment.expected
  toReturn$payment.process.cost[past.indexes] = toReturn$payment.process.cost[past.indexes] + hist.leases$payment.process.cost
  toReturn$asset.acq.outflow[past.indexes] = toReturn$asset.acq.outflow[past.indexes] + hist.leases$asset.acq.outflow
  toReturn$total.assets.linear[past.indexes] = toReturn$total.assets.linear[past.indexes] + hist.leases$total.assets.linear
  toReturn$total.assets.loan[past.indexes] = toReturn$total.assets.loan[past.indexes] + hist.leases$total.assets.loan
  toReturn$lease.depreciation[past.indexes] = toReturn$lease.depreciation[past.indexes] + hist.leases$lease.depreciation
  toReturn$bad.debt[past.indexes] = toReturn$bad.debt[past.indexes] + hist.leases$bad.debt
  toReturn$leases.booked[past.indexes] = toReturn$leases.booked[past.indexes] + hist.leases$leases.booked
  toReturn$ongoing.leases[past.indexes] = toReturn$ongoing.leases[past.indexes] + hist.leases$ongoing.leases

  # Third party data costs increase with number of apps, so they increase whenever a new lease happens
  toReturn$third.pt.data.cost[past.indexes] <- toReturn$third.pt.data.cost[past.indexes] +
    toReturn$leases.booked[past.indexes]*assumptions$third.pt.data/assumptions$app.conversion.rt

  # Legal costs generally increases with growth of business, small constant ties it to each lease
  toReturn$legal.cost[past.indexes] <- toReturn$legal.cost[past.indexes] + toReturn$leases.booked[past.indexes]*assumptions$add.legal.cost

  past.months <- unique(format.Date(toReturn$dates[past.indexes], format = "%m %y"))
  for (b in 1 : length(past.months)){
    this.month <- which(format.Date(toReturn$dates, format = "%m %y") == past.months[b])

    toReturn$apps.per.month[this.month] <- toReturn$apps.per.month[this.month] + sum(toReturn$leases.booked[this.month]) /  assumptions$app.conversion.rt
    toReturn$it.cost[this.month] <- toReturn$it.cost[this.month] + (sum(toReturn$leases.booked[this.month]) /  assumptions$app.conversion.rt)^.7 * assumptions$it.rate / 365 * 12
  }

  for (x in 1: length(lease.growth$date)) {
    start.index <- which(toReturn$dates == lease.growth$date[x])

    rel.indexes1 <- which(average.lease$payment.due != 0)
    toReturn$payment.due[start.index + rel.indexes1 - 1] <-
      toReturn$payment.due[start.index + rel.indexes1 - 1] +
      lease.growth$booked[x]*average.lease$payment.due[rel.indexes1]

    rel.indexes2 <- which(average.lease$payment.expected != 0)
    toReturn$payment.expected[start.index + rel.indexes2 - 1] <-
      toReturn$payment.expected[start.index + rel.indexes2 - 1] +
      lease.growth$booked[x]*average.lease$payment.expected[rel.indexes2]

    rel.indexes3 <- which(average.lease$asset.acq.outflow != 0)
    toReturn$asset.acq.outflow[start.index + rel.indexes3 - 1] <-
      toReturn$asset.acq.outflow[start.index + rel.indexes3 - 1] +
      lease.growth$booked[x]*average.lease$asset.acq.outflow[rel.indexes3]

    rel.indexes4 <- which(average.lease$payment.process.cost != 0)
    toReturn$payment.process.cost[start.index + rel.indexes4 - 1] <-
      toReturn$payment.process.cost[start.index + rel.indexes4 - 1] +
      lease.growth$booked[x]*average.lease$payment.process.cost[rel.indexes4]

    rel.indexes5 <- which(average.lease$total.assets.linear != 0)
    toReturn$total.assets.linear[start.index + rel.indexes5 - 1] <-
      toReturn$total.assets.linear[start.index + rel.indexes5 - 1] +
      lease.growth$booked[x]*average.lease$total.assets.linear[rel.indexes5]

    rel.indexes6 <- which(average.lease$total.assets.loan != 0)
    toReturn$total.assets.loan[start.index + rel.indexes6 - 1] <-
      toReturn$total.assets.loan[start.index + rel.indexes6 - 1] +
      lease.growth$booked[x]*average.lease$total.assets.loan[rel.indexes6]

    rel.indexes7 <- which(average.lease$lease.depreciation != 0)
    toReturn$lease.depreciation[start.index + rel.indexes7 - 1] <-
      toReturn$lease.depreciation[start.index + rel.indexes7 - 1] +
      lease.growth$booked[x]*average.lease$lease.depreciation[rel.indexes7]

    rel.indexes8 <- which(average.lease$payments.sum != 0)
    toReturn$payments.sum[start.index + rel.indexes8 - 1] <-
      toReturn$payments.sum[start.index + rel.indexes8 - 1] +
      lease.growth$booked[x]*average.lease$payments.sum[rel.indexes8]

    # Third party data costs increase with number of apps, so they increase whenever a new lease happens
    toReturn$third.pt.data.cost[start.index] <- toReturn$third.pt.data.cost[start.index] +
      lease.growth$booked[x]*assumptions$third.pt.data/assumptions$app.conversion.rt

    # Legal costs generally increases with growth of business, small constant ties it to each lease
    toReturn$legal.cost[start.index] <- toReturn$legal.cost[start.index] + lease.growth$booked[x]*assumptions$add.legal.cost

    # Leases booked
    toReturn$leases.booked[which(toReturn$dates == lease.growth$date[x])] <- toReturn$leases.booked[which(toReturn$dates == lease.growth$date[x])] +
      lease.growth$booked[x]

    # Applications for month, for each lease started in that month increment apps per month by 1 divided by app-to-lease conversion rate
    relevant.dates <- which(format.Date(toReturn$dates, format = "%m %y") == format.Date(lease.growth$date[x], format = "%m %y"))
    toReturn$apps.per.month[relevant.dates] <- toReturn$apps.per.month[relevant.dates] + lease.growth$booked[x]/ assumptions$app.conversion.rt

    # Number of ongoing leases on any day
    toReturn$ongoing.leases[which(toReturn$dates >= lease.growth$date[x] &
                                    toReturn$dates < (lease.growth$date[x] + assumptions$avg.lease.length * 365 / 12))] <-
      toReturn$ongoing.leases[which(toReturn$dates >= lease.growth$date[x] &
                                      toReturn$dates < (lease.growth$date[x] + assumptions$avg.lease.length * 365 / 12))] + lease.growth$booked[x]

    # additional monthly IT cost depending on number of new applications in that month
    relevant.dates2 <- which(format.Date(toReturn$dates, format = "%m %y") == format.Date(lease.growth$date[x], format = "%m %y"))
    toReturn$it.cost[relevant.dates2] <- toReturn$it.cost[relevant.dates2] + toReturn$apps.per.month[relevant.dates2]^.7 * assumptions$it.rate / 365 * 12
  }

  # First of month costs
  toReturn$payment.process.cost[which(format.Date(toReturn$dates, format = "%d") == "01")] <- assumptions$pay.process.month

  toReturn$rent.cost[which(format.Date(toReturn$dates, format = "%d") == "01")] <- assumptions$rent.base

  # Accounting for employees

  # Hired Employees, not csm or salespeople
  emp.sheet$start.date <- as.Date(emp.sheet$start.date, "%m/%d/%y")
  emp.sheet$end.date <- as.Date(emp.sheet$end.date, "%m/%d/%y")
  for (i in 1:nrow(emp.sheet)) {
    toReturn$no.employees[which(toReturn$dates >= as.vector(emp.sheet[i,]$start.date) &
                                  toReturn$dates < as.vector(emp.sheet[i,]$end.date))] <-
      toReturn$no.employees[which(toReturn$dates >= as.vector(emp.sheet[i,]$start.date) &
                                    toReturn$dates < as.vector(emp.sheet[i,]$end.date))] + 1

    toReturn$other.salaries[which(toReturn$dates >= as.vector(emp.sheet[i,]$start.date) &
                                    toReturn$dates < as.vector(emp.sheet[i,]$end.date))] <-
      toReturn$other.salaries[which(toReturn$dates >= as.vector(emp.sheet[i,]$start.date) &
                                      toReturn$dates < as.vector(emp.sheet[i,]$end.date))] +
      emp.sheet[i,]$salary / 365
  }

  firsts.of.month <- toReturn$dates[which(format.Date(toReturn$dates, format = "%d") == "01" &
                                            format.Date(toReturn$dates, format = "%m %y") != format.Date(first.of.month, format = "%m %y"))]
  #Average.apps.per.day
  for (i in seq_along(c(first.of.month,firsts.of.month))) {
    toReturn$average.app.per.day[which(format.Date(toReturn$dates, format = "%m %y") == format.Date(c(first.of.month,firsts.of.month)[i], format = "%m %y"))] =
      sum(toReturn$leases.booked[which(format.Date(toReturn$dates, format = "%m %y") == format.Date(c(first.of.month,firsts.of.month)[i], format = "%m %y"))] / assumptions$app.conversion.rt)/
      length(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(c(first.of.month,firsts.of.month)[i], format = "%m %y")))
  }

  #Change in average apps per day
  toReturn$average.app.per.day.diff[which(format.Date(toReturn$dates, format= "%m %y") == format.Date(first.of.month, format = "%m %y"))] <- toReturn$average.app.per.day[1]
  for (i in seq_along(firsts.of.month)){
    toReturn$average.app.per.day.diff[which(format.Date(toReturn$dates, format = "%m %y") == format.Date(firsts.of.month[i], format = "%m %y"))] <-
      toReturn$average.app.per.day[which(toReturn$dates == firsts.of.month[i])] - toReturn$average.app.per.day[which(toReturn$dates == firsts.of.month[i]) - 1]
  }

  # Number of customer service managers, tied to the number of ongoing leases
  toReturn$no.csm[which(toReturn$ongoing.leases != 0)] <- ceiling(toReturn$ongoing.leases[which(toReturn$ongoing.leases != 0)] / assumptions$leases.per.csm)

  # Number of salespeople, tied to apps per month and app difference per month
  toReturn$no.salesperson[which(toReturn$average.app.per.day.diff > 0)] <- ceiling(toReturn$average.app.per.day[which(toReturn$average.app.per.day.diff > 0)] / assumptions$apps.per.salesp +
                                                                                     toReturn$average.app.per.day.diff[which(toReturn$average.app.per.day.diff > 0)] / assumptions$n.apps.per.salesp)
  toReturn$no.salesperson[which(toReturn$average.app.per.day.diff<= 0)] <-  ceiling(toReturn$average.app.per.day[which(toReturn$average.app.per.day.diff<= 0)] / assumptions$apps.per.salesp)

  # increment number of employees and total salaries based on number of csm and salespeople
  toReturn$no.employees <- toReturn$no.employees + toReturn$no.csm + toReturn$no.salesperson

  toReturn$csm.salaries <- floor(toReturn$total.salaries + toReturn$no.csm * assumptions$csm.salary / 365 * 100) / 100
  toReturn$sales.salaries <- floor(toReturn$no.salesperson * assumptions$sales.salary / 365 * 100) / 100
  toReturn$total.salaries <- toReturn$csm.salaries + toReturn$sales.salaries + toReturn$other.salaries

  # Employee benefits
  toReturn$employee.tax <- toReturn$total.salaries * assumptions$employee.tax
  toReturn$employee.benefit <- toReturn$total.salaries * assumptions$employee.benefits

  # Rent per employee per day (cubicle costs)
  toReturn$rent.cost <- toReturn$rent.cost + toReturn$no.employees * 3.25

  # Add up all the costs
  toReturn$ops.cost <- toReturn$payment.process.cost + toReturn$total.salaries + toReturn$employee.tax +
    toReturn$employee.benefit + toReturn$rent.cost + toReturn$third.pt.data.cost + toReturn$it.cost + toReturn$legal.cost

  # Round everything
  toReturn$apps.per.month <- round(toReturn$apps.per.month)
  toReturn$average.app.per.day <- floor(toReturn$average.app.per.day * 100) / 100
  toReturn$average.app.per.day.diff <- floor(toReturn$average.app.per.day.diff * 1000) / 1000
  toReturn$payment.due <- floor(toReturn$payment.due * 1000)/1000
  toReturn$payment.expected <- floor(toReturn$payment.expected * 100) / 100
  toReturn$payment.process.cost <- floor(toReturn$payment.process.cost * 100) / 100
  toReturn$asset.acq.outflow <- floor(toReturn$asset.acq.outflow * 100) / 100
  toReturn$total.assets.linear <- floor(toReturn$total.assets.linear * 100) / 100
  toReturn$total.assets.loan <- floor(toReturn$total.assets.loan * 100) / 100
  toReturn$lease.depreciation <- floor(toReturn$lease.depreciation * 100) / 100
  toReturn$third.pt.data.cost <- floor(toReturn$third.pt.data.cost * 100) / 100
  toReturn$rent.cost <- floor(toReturn$rent.cost * 100)/100
  toReturn$it.cost <- floor(toReturn$it.cost * 100)/100
  toReturn$legal.cost <- floor(toReturn$legal.cost * 100) / 100
  toReturn$ops.cost <- floor(toReturn$ops.cost * 100) / 100

  toReturn$cash.flow <- toReturn$payment.expected - toReturn$ops.cost - toReturn$asset.acq.outflow

  toReturn <- filter(toReturn, dates < last(lease.growth$date))

  temp = data.frame(dates = c(first.of.month, firsts.of.month[which(firsts.of.month < last(lease.growth$date))]),
                    #                    month = 1:length(c(first.of.month, firsts.of.month[which(firsts.of.month < last(lease.growth$date))])),
                    apps.per.month = 0,
                    leases.booked = 0,
                    average.app.per.day = 0,
                    average.app.per.day.diff = 0,
                    ongoing.leases = 0,
                    payment.due = 0,
                    payment.expected = 0,
                    asset.acq.outflow = 0,
                    total.assets.linear = 0,
                    total.assets.loan = 0,
                    lease.depreciation = 0,
                    payment.process.cost = 0,
                    no.employees = 0,
                    no.csm = 0,
                    no.salesperson = 0,
                    total.salaries = 0,
                    other.salaries = 0,
                    sales.salaries = 0,
                    csm.salaries = 0,
                    employee.tax = 0,
                    employee.benefit = 0,
                    rent.cost = 0,
                    third.pt.data.cost = 0,
                    it.cost = 0,
                    legal.cost = 0,
                    ops.cost = 0,
                    cash.flow = 0,
                    payments.sum = 0
  )
  for (i in 1:nrow(temp)){
    temp$apps.per.month[i] <- toReturn$apps.per.month[(which(toReturn$dates == temp$dates[i]))]
    temp$leases.booked[i] <- sum(toReturn$leases.booked[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$average.app.per.day[i] <-toReturn$average.app.per.day[(which(toReturn$dates == temp$dates[i]))]
    temp$average.app.per.day.diff[i] <-toReturn$average.app.per.day.diff[(which(toReturn$dates == temp$dates[i]))]
    temp$ongoing.leases[i] <- round(sum(toReturn$ongoing.leases[which(toReturn$dates == temp$dates[i])]) / length(which(toReturn$dates == temp$dates[i])))
    temp$payment.due[i] <- sum(toReturn$payment.due[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$payment.expected[i] <- sum(toReturn$payment.expected[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$asset.acq.outflow[i] <- sum(toReturn$asset.acq.outflow[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$total.assets.linear[i] <- toReturn$total.assets.linear[(max(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y"))))]
    temp$total.assets.loan[i] <- toReturn$total.assets.loan[(max(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y"))))]
    temp$lease.depreciation[i] <- sum(toReturn$lease.depreciation[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$payment.process.cost[i] <- sum(toReturn$payment.process.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$no.employees[i] <- round(sum(toReturn$no.employees[which(toReturn$dates == temp$dates[i])]) / length(which(toReturn$dates == temp$dates[i])))
    temp$no.csm[i] <- round(sum(toReturn$no.csm[which(toReturn$dates == temp$dates[i])]) / length(which(toReturn$dates == temp$dates[i])))
    temp$no.salesperson[i] <- round(sum(toReturn$no.salesperson[which(toReturn$dates == temp$dates[i])]) / length(which(toReturn$dates == temp$dates[i])))
    temp$total.salaries[i] <- sum(toReturn$total.salaries[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$other.salaries[i] <- sum(toReturn$other.salaries[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$csm.salaries[i] <- sum(toReturn$csm.salaries[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$sales.salaries[i] <- sum(toReturn$sales.salaries[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$employee.tax[i] <- sum(toReturn$employee.tax[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$employee.benefit[i] <- sum(toReturn$employee.benefit[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$rent.cost[i] <- sum(toReturn$rent.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$third.pt.data.cost[i] <- sum(toReturn$third.pt.data.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$it.cost[i] <- sum(toReturn$it.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$legal.cost[i] <- sum(toReturn$legal.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$ops.cost[i] <- sum(toReturn$ops.cost[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$cash.flow[i] <- sum(toReturn$cash.flow[(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y")))])
    temp$payments.sum[i] <- toReturn$payments.sum[(max(which(format.Date(toReturn$dates, format = "%m %y") == format.Date(temp$dates[i], format = "%m %y"))))]

    #    temp$year =  ceiling((temp$month-6)/12) - 1
  }
  toReturn <- temp
  toReturn
}

test.cash.flow = cash.flow()

#' The debt.calc function
#'
#' This function calculates debt metrics given cash flow data
#'
#' @param df, the data frame output from the cash.flow function
#' @param assumptions, a list of constant values
#' @param il.sheet, information about interest and leverage
#' @return a data frame with debt metrics added on to cash flow metrics
#' @export
debt.calc <- function (df = test.cash.flow,
                       assumptions = default.constants.list,
                       il.sheet = default.int.lev) {
  total.assets = df$total.assets.linear
  find.index <- function(a.date){
    toReturn = 1
    for (i in 2: length(il.sheet$Date)) {
      if (a.date >=  as.Date(il.sheet$Date[i], "%m/%d/%Y")){
        toReturn = i
      }
    }
    toReturn
  }

  il.sheet.indexes <- sapply(df$dates, find.index)

  debt.borrowed <- if (assumptions$depr.method == 2) df$total.assets.loan * il.sheet$Leverage[il.sheet.indexes]
  else if (assumptions$depr.method == 1) df$total.assets.linear * il.sheet$Leverage[il.sheet.indexes]
  debt.difference <- c(debt.borrowed[1],
                       diff(debt.borrowed))
  debt.cost <- debt.borrowed * (il.sheet$Interest[il.sheet.indexes]  / 12)

  asset.change <- rep(0, length(debt.borrowed))
  asset.change[1] = total.assets[1]
  for (i in 2:length(asset.change)){
    asset.change[i] = total.assets[i] - total.assets[i-1]
  }
  df$ebitda = df$payment.expected - df$ops.cost - df$asset.acq.outflow + asset.change
  df$gross.profit = df$ebitda + df$ops.cost
  df$pre.tax.profit = floor((df$ebitda - debt.cost) * 100) / 100
  calendar.years = unique(format.Date(df$dates, format = "%y"))
  profit.by.year = rep(0,length(calendar.years))
  for (i in 1: length(calendar.years)){
    profit.by.year[i] = sum(df$pre.tax.profit[which(format.Date(df$dates, format = "%y") == calendar.years[i])])
  }
  income.tax = rep(0, length(profit.by.year))
  income.tax[1] = if (profit.by.year[1] > 0) profit.by.year[1] * fed.tax.rate else 0
  for (i in 2: length(income.tax)){
    if (sum(profit.by.year[1:i]) < 0) income.tax[i] = 0
    else if (sum(profit.by.year[1:i]) > 0 && income.tax[i-1] == 0) income.tax[i] = sum(profit.by.year[1:i]) * assumptions$fed.tax.rate
    else if (sum(profit.by.year[1:i]) > 0  && income.tax[i-1] > 0) income.tax[i] = profit.by.year[i] * assumptions$fed.tax.rate
  }
  df$taxed.income = rep(0, length(df$dates))
  for (i in 1: length(profit.by.year)) {
    rel.months <- which((format.Date(df$dates, format = "%m-%y") == paste("03-",calendar.years[i], sep = "") |
                           format.Date(df$dates, format = "%m-%y") == paste("06-",calendar.years[i], sep = "") |
                           format.Date(df$dates, format = "%m-%y") == paste("09-",calendar.years[i], sep = "") |
                           format.Date(df$dates, format = "%m-%y") == paste("12-",calendar.years[i], sep = "")))
    df$taxed.income[rel.months] = floor(income.tax[i] / 4 * 100) / 100
  }
  df$net.income = df$pre.tax.profit - df$taxed.income

  df$cash.flow = df$ cash.flow - df$taxed.income
  df$total.assets = floor(total.assets)
  df$cash.flow.debt = floor((df$cash.flow + debt.difference - debt.cost) * 100)/100
  df$debt.interest = floor(debt.cost * 100) / 100
  df$debt.borrowed = floor(debt.borrowed * 100) / 100

  df$cum.cash.with.debt <- cumsum(df$cash.flow.debt)
  df$cum.cash.without.debt <- cumsum(df$cash.flow)
  df$MoneyForEmp <- df$payment.expected - df$asset.acq.outflow +
    debt.difference -
    df$payment.process.cost - df$rent.cost - df$third.pt.data.cost -
    df$it.cost - df$legal.cost
  df
}

test.debt.calc = debt.calc()

#' The output.subset function
#'
#' This function subsets a data frame to a desired time frame
#'
#' @param df, the data frame to subset
#' @param first.month, the first month to be shown
#' @param last.month, the last month to be shown
#' @return the subset of df within first.month and last.month
#' @export
output.subset <- function(df= test.debt.calc, first.month=default.output.start.month, last.month=default.output.end.month) {
  out.df <- df[which(df$dates >= first.month & df$dates <= last.month),]
  out.df$month <- c(1:nrow(out.df))
  out.df$year <- ceiling((out.df$month)/12)
  return(out.df)
}

test.output.subset <- output.subset()

#' The mthSummary function
#'
#' Summarizes cash flow data by month
#'
#' @param df, cash flow data
#' @return a data.frame with data organized by month
#' @export
mthSummary <- function(df = test.output.subset){
  toReturn <- df %>%
    group_by(month) %>%
    summarise(Date = dates,
              EmployeesMonthEnd = no.employees,
              AppsPerMonth = apps.per.month,
              TotalLeasesBooked = leases.booked,
              OngoingLeases = ongoing.leases,
              TotalLeaseAssetsMonthEnd = total.assets,
              SumOfPaymentsDue = payments.sum,
              PaymentDue = payment.due,
              Revenue = payment.expected,
              LeaseDepreciation = lease.depreciation,
              GrossProfit = gross.profit,
              TotalExpenses = ops.cost,
              MoneyForEmployee = MoneyForEmp,
              ._TotalLabor = total.salaries + employee.tax + employee.benefit,
              .___LaborSales = sales.salaries,
              .___LaborServicing = csm.salaries,
              .___LaborNonOps = other.salaries,
              .___LaborBenefitsTaxes = employee.tax + employee.benefit,
              ._Rent = rent.cost,
              ._Legal = legal.cost,
              ._ITinfrastructure = it.cost,
              ._ThirdPartyData = third.pt.data.cost,
              ._PaymentProcessing = payment.process.cost,
              EBITDA = ebitda,
              ._Interest = debt.interest,
              Profit = pre.tax.profit,
              LeaseAcquisition = asset.acq.outflow,
              CashFlowWithoutDebt = cash.flow + taxed.income,
              DebtBorrowed = debt.borrowed,
              CashFlowWithDebt = cash.flow.debt  + taxed.income
    )
  toReturn
}

test.mthSummary <- mthSummary()

#' The yrSummary function
#'
#' Summarizes cash flow data by year
#'
#' @param df, cash flow data
#' @return a data.frame with data organized by year
#' @export
yrSummary <- function(df = test.output.subset){
  toReturn <- df %>%
    group_by(year) %>%
    summarise(EmployeesYearEnd = no.employees[which.max(dates)],
              TotalLeasesBooked = sum(leases.booked),
              TotalLeaseAssetsYearEnd = total.assets[which.max(dates)],
              SumOfPaymentsDue = payments.sum[which.max(dates)],
              Revenue = sum(payment.expected),
              LeaseDepreciation = sum(lease.depreciation), #"not sure"
              GrossProfit = sum(gross.profit),
              TotalExpenses = sum(ops.cost),
              ._TotalLabor = sum(total.salaries) + sum(employee.tax) + sum(employee.benefit),
              .___LaborSales = sum(sales.salaries),
              .___LaborServicing = sum(csm.salaries),
              .___LaborNonOps = sum(other.salaries),
              .___LaborBenefitsTaxes = sum(employee.tax) + sum(employee.benefit),
              ._Rent = sum(rent.cost),
              ._Legal = sum(legal.cost),
              ._ITinfrastructure = sum(it.cost),
              ._ThirdPartyData = sum(third.pt.data.cost),
              ._PaymentProcessing = sum(payment.process.cost),
              EBITDA = sum(ebitda), #not sure
              ._Interest = sum(debt.interest),
              PreTaxProfit = sum(pre.tax.profit), #not sure
              ._Taxes = sum(taxed.income),
              NetIncome = sum(net.income),
              LeaseAcquisition = sum(asset.acq.outflow), #not sure
              CashFlowWithoutDebt = sum(cash.flow),
              DebtBorrowed = debt.borrowed[which.max(dates)],
              CashFlowWithDebt = sum(cash.flow.debt)
    )
  toReturn$SalesExpense_NumLeases = floor(toReturn$.___LaborSales / toReturn$TotalLeasesBooked * 100) / 100
  toReturn$Labor_Assets = floor(toReturn$._TotalLabor / toReturn$TotalLeaseAssetsYearEnd * 100) / 100
  toReturn$TotalExpenses_Assets = floor(toReturn$TotalExpenses / toReturn$TotalLeaseAssetsYearEnd * 100) / 100
  toReturn$Revenue_Assets = floor(toReturn$Revenue / toReturn$TotalLeaseAssetsYearEnd * 100) / 100
  toReturn$TotalExpenses_Revenue = floor(toReturn$TotalExpenses / toReturn$Revenue * 100) / 100
  toReturn$EBITDA_Revenue = floor(toReturn$EBITDA / toReturn$Revenue * 100) / 100

  toReturn
}

test.yrSummary <- yrSummary()

#' The lease.returns function
#'
#' This calculates 12 month, 18 month, and total returns given average lease data
#'
#' @param a.lease, data on cash metrics of the average lease
#' @param assumptions, a list of constant values
#' @return a list with 12 month, 18 month, and total returns
#' @export
lease.returns <- function(a.lease = test.average.lease,
                          assumptions = default.constants.list){
  toReturn <- list()
  toReturn$twelve.m.return <- floor(sum(a.lease$payment.expected[c(1:365)]) / (sum(a.lease$asset.acq.outflow[c(1:365)])) /(1+assumptions$sales.tax)* 100) / 100
  toReturn$eighteen.m.return <- floor(sum(a.lease$payment.expected[c(1:547)]) / (sum(a.lease$asset.acq.outflow[c(1:547)])) /(1+assumptions$sales.tax)* 100) / 100
  toReturn$total.return <- floor(sum(a.lease$payment.expected) / (sum(a.lease$asset.acq.outflow)) /(1+assumptions$sales.tax)* 100) / 100
  toReturn
}

#' The irr.day function
#'
#' Calculates IRR on a by-day basis
#'
#' @param df, average.lease data
#' @param no.days, number of days to calculate for
#' @return a number, the IRR
#' @export
irr.day <- function(df = test.average.lease,
                    no.days = 1000) {
  f <- function(x){
    cashflows <- sum(df$payment.expected / (1 + x)^(1:no.days))
    cashflows - sum(df$asset.acq.outflow)
  }
  uniroot(f, c(-.5,1), tol=.000001)$root  * 365
}

#' The irr.month function
#'
#' Calculates IRR on a by-month basis
#'
#' @param df, average.lease data
#' @param no.days, number of days to calculate for
#' @return a number, the IRR
#' @export
irr.month <- function(df = test.average.lease,
                      no.months = 24) {
  f <- function (y){
    days.index = (1:no.months) * 28
    cashflows = rep(0, no.months)
    cashflows[1] = sum(df$payment.expected[which(df$days <= days.index[1])])
    for (z in 2 : no.months){
      cashflows[z] = sum(df$payment.expected[which(df$days <= days.index[z] & df$days > days.index[z-1])])
    }
    cashflows2 <- sum(cashflows / ((1 + y)^((1:no.months)-0.5)))
    cashflows2 - sum(df$asset.acq.outflow)
  }
  uniroot(f, c(-.6,.2))$root * 13
}

#' The cash.by.day function
#'
#' Graphs cumulative cash received per lease by day
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @param assumptions, a list of constant values
#' @return The graph of cumulative cash received per lease by day
#' @export
cash.by.day <- function(a.lease = test.average.lease,
                        file = NULL,
                        text.size = 12,
                        small.text.size = 4,
                        assumptions = default.constants.list) {
  cash <- cumsum(a.lease$payment.expected / (1+assumptions$sales.tax) - a.lease$asset.acq.outflow)
  df <- data.frame(days = 1:1000, cash = cash)
  toReturn <- (ggplot(data = df, aes(x = days)) + geom_line(aes(y = cash, colour = "cumulative cash"))) + theme_bw()
  toReturn$labels$x <- "Time in Days"
  toReturn$labels$y <- "Cumulative Cash"
  toReturn$labels$title <- "Cumulative Cash over Time"
  toReturn$labels$colour <- ""
  toReturn <- toReturn + geom_text(x = 160, y = -1050, label = paste("Min:", as.character(floor(min(toReturn$data$cash) * 100)/100), "on day", as.character(which.min(toReturn$data$cash))), size = small.text.size) +
    geom_text(x = 600, y = 420, label = paste("Max:", as.character(floor(max(toReturn$data$cash) * 100)/100), "from day", as.character(which.max(toReturn$data$cash)), "on"), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The cash.by.month function
#'
#' Graphs cumulative cash received per lease by month
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @param assumptions, a list of constant values
#' @return The graph of cumulative cash received per lease by month
#' @export
cash.by.month <- function(a.lease = test.average.lease,
                          file = NULL,
                          text.size = 12,
                          small.text.size = 4,
                          assumptions = default.constants.list) {
  cash <- cumsum(a.lease$payment.expected / (1+assumptions$sales.tax)- a.lease$asset.acq.outflow)
  df <- data.frame(months = 1:26, cash = cash[(1:26) * 28 - 14])
  toReturn <- (ggplot(data = df, aes(x = months)) + geom_line(aes( y = cash, colour = "cumulative cash"))) + theme_bw()
  toReturn$labels$x <- "Time in Months"
  toReturn$labels$y <- "Cumulative Cash"
  toReturn$labels$title <- "Cumulative Cash over Time"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 6, y = -1050, label = paste("Min:", as.character(floor(min(toReturn$data$cash) * 100)/100), "on month", as.character(which.min(toReturn$data$cash))), size = small.text.size) +
    geom_text(x = 21, y = 400, label = paste("Max:", as.character(floor(max(toReturn$data$cash) * 100)/100), "from month", as.character(which.max(toReturn$data$cash)), "on"), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The cash.by.month function
#'
#' Graphs cumulative cash received per lease by month
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @return The graph of cumulative cash received per lease by month
#' @export
expected.vs.due <- function(a.lease = test.average.lease,
                            file = NULL,
                            text.size = 12,
                            small.text.size = 4) {
  expected <- cumsum(a.lease$payment.expected)
  due <- cumsum(a.lease$payment.due)
  df <- data.frame(months = 1:26, expected = expected[(1:26) * 28], due = due[(1:26)*28])
  toReturn <- (ggplot(df, aes(months)) +
                 geom_line(aes(y = expected, colour = "actual payment")) +
                 geom_line(aes(y = due, colour = "payment due"))) + theme_bw()
  toReturn$labels$x <- "Time in Months"
  toReturn$labels$y <- "Cumulative Cash"
  toReturn$labels$title <- "Expected Cash Flow vs. Cash Flow Due"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 7, y = 300, label = paste("Min:",as.character(floor(min(toReturn$data$due) * 100) / 100), "on month", as.character(which.min(toReturn$data$due))), size = small.text.size, colour = 5) +
    geom_text(x = 7, y = 400, label = paste("Min:",as.character(floor(min(toReturn$data$expected) * 100) / 100), "on month", as.character(which.min(toReturn$data$expected))), size = small.text.size, colour = 2) +
    geom_text(x = 21, y = 2350, label = paste("Max:",as.character(floor(max(toReturn$data$due) * 100) / 100), "from month", as.character(which.max(toReturn$data$due)), "on"), size = small.text.size, colour = 5) +
    geom_text(x = 21, y = 1700, label = paste("Max:",as.character(floor(max(toReturn$data$expected) * 100) / 100), "from month", as.character(which.max(toReturn$data$expected)), "on"), size = small.text.size, colour = 2)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The bad.debt.month function
#'
#' Graphs bad debt received as a percentage of asset value on a by-month basis
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @return The graph of bad debt by month
#' @export
bad.debt.month <- function (a.lease = test.average.lease,
                            file = NULL,
                            text.size = 12,
                            small.text.size = 4) {
  bad.debt <- c(rep(0,120), cumsum(a.lease$bad.debt / sum(a.lease$asset.acq.outflow)))
  df <- data.frame(months = 1:30, bad.debt = bad.debt[(1:30)*28])
  toReturn <- (ggplot(data = df, aes(x = months)) +  geom_line(aes(y = bad.debt, colour = "bad debt as % of assets"))) + theme_bw()
  toReturn$labels$x <- "Time in Months"
  toReturn$labels$y <- "Bad Debt as Percent of Assets"
  toReturn$labels$title <- "Cumulative Bad Debt Over Time"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 12, y = 0, label = paste("Min: 0 from months", as.character(min(which(toReturn$data$bad.debt == 0))), "to", as.character(max(which(toReturn$data$bad.debt == 0)))), size = small.text.size) +
    geom_text(x = 25, y = .26, label = paste("Max:", as.character(floor(max(toReturn$data$bad.debt) * 100) / 100), "from month", as.character(which.max(toReturn$data$bad.debt)), "on"), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The bad.debt.to.pay function
#'
#' Graphs bad debt received as a percentage of payment expected at that point
#'   on a by-month basis
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @return The graph of bad debt by month
#' @export
bad.debt.to.pay <- function (a.lease = test.average.lease,
                             file = NULL,
                             text.size = 12,
                             small.text.size = 4){
  toReturn <- ggplot(data = a.lease, aes(x = days)) +
    geom_line(aes(y = cumsum(bad.debt) / cumsum(payment.expected),
                  colour = "Bad debt as cumulative % of expected payments")) + theme_bw()
  toReturn$labels$x <- "Time in Days"
  toReturn$labels$y <- "% of expected payment that has defaulted"
  toReturn$labels$title <- "Bad Debt to Payments over time"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 680, y = .23, label = paste(as.character(floor(max(cumsum(a.lease$bad.debt) / cumsum(a.lease$payment.expected)) * 100)), "% of payers default by day",
                                              as.character(which.max(cumsum(a.lease$bad.debt) / cumsum(a.lease$payment.expected)))), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The bad.debt.total.pdue function
#'
#' Graphs bad debt received as a percentag of total payments due on a by-month basis
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @return The graph of bad debt by month
#' @export
bad.debt.total.pdue <- function (a.lease = test.average.lease,
                                 file = NULL,
                                 text.size = 12,
                                 small.text.size = 4) {
  bad.debt <- c(rep(0,120), cumsum(a.lease$bad.debt / sum(a.lease$payment.due)))
  df <- data.frame(months = 1:30, bad.debt = bad.debt[(1:30)*28])
  toReturn <- (ggplot(data = df, aes(x = months)) +  geom_line(aes(y = bad.debt, colour = "bad debt as % of total payments due"))) + theme_bw()
  toReturn$labels$x <- "Time in Months"
  toReturn$labels$y <- "Bad Debt as Percent of Total Payments Due"
  toReturn$labels$title <- "Bad Debt Over Payment.due"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 12, y = 0, label = paste("Min: 0 from months", as.character(min(which(toReturn$data$bad.debt == 0))), "to", as.character(max(which(toReturn$data$bad.debt == 0)))), size = small.text.size) +
    geom_text(x = 25, y = .16, label = paste("Max:", as.character(floor(max(toReturn$data$bad.debt) * 100) / 100), "from month", as.character(which.max(toReturn$data$bad.debt)), "on"), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}

#' The cum.default.rate function
#'
#' Graphs the cumulative % of payers who have defaulted by month.
#'
#' @param a.lease, average lease data
#' @param file, if this isn't null, the graph will be saved as this file
#' @param text.size, text size for graph titles and sides
#' @param small.text.size, text size for the smaller text inside the graph
#' @param assumptions, a list of constant values
#' @param default.rate, a vector representing the default rate, must be 1000 in length with values between 0 and 1
#' @return The graph of default rate by month
#' @export
cum.default.rate <- function (a.lease = test.average.lease,
                              file = NULL,
                              text.size = 12,
                              small.text.size = 4,
                              assumptions = default.constants.list,
                              default.rate = norm.default.rate) {
  df <- data.frame(months = 1:assumptions$avg.lease.length, default.rate = 1- cumprod(default.rate[(0:(assumptions$avg.lease.length-1))*28 + 1]))
  toReturn <- (ggplot(data = df, aes(x = months)) +  geom_line(aes(y = default.rate, colour = "% of payers who have defaulted"))) + theme_bw()
  toReturn$labels$x <- "Time in Months"
  toReturn$labels$y <- "% of payers who have defaulted"
  toReturn$labels$title <- "Cumulative Default Rate over time"
  toReturn$labels$colour <- ""
  toReturn <- toReturn +
    geom_text(x = 11, y = .23, label = paste(as.character(floor(max(toReturn$data$default.rate) * 100)), "% of payers default by month", as.character(assumptions$avg.lease.length)), size = small.text.size)
  toReturn$theme$text$size = text.size
  if (!is.null(file)){
    ggsave(toReturn, filename = file, width = 5, height = 2.5)
  }
  toReturn
}


#' The saveToSheet function
#'
#' This function writes the results into an excel spreadsheet and saves it
#'
#' @param file, the name of the file the sheet will be saved as. If null the file won't be esaved
#' and the workbook will be returned.
#' @param yrdata, cash flow information by year, from the yrSummary function
#' @param mthdata, cash flow information by year, from the mthSummary function
#' @param assumptions, constants represented in the original data frame form
#' @param emp.data, data on employees
#' @param growth.data, growth.rates data
#' @return a data frame with debt metrics added on to cash flow metrics
#' @export
saveToSheet <- function(file = NULL,
                        yrdata = test.yrSummary,
                        mthdata = test.mthSummary,
                        assumptions = default.constants.table,
                        emp.data = default.employees,
                        growth.data = default.growth.rates){

  yrtemp  <- data.frame(
    Projections = colnames(yrdata)[2:length(colnames(yrdata))]
  )

  for (i in 1 : nrow(yrdata)) {
    yrtemp[,paste("Year_", i, sep = "")] <- as.numeric(yrdata[i,][2:length(colnames(yrdata))])
  }

  mthdates <- data.frame(
    Projections = colnames(mthdata)[2]
  )

  mthtemp <- data.frame(
    Projections = colnames(mthdata)[3:length(colnames(mthdata))]
  )

  for (i in 1 : nrow(mthdata)){
    mthdates[,paste("Month_",i,sep = "")] <- mthdata[i,][2]
    mthtemp[,paste("Month_",i,sep = "")] <- as.numeric(mthdata[i,][3:length(colnames(mthdata))])
  }

  # Write final results to one big spreadsheet
  wb <- createWorkbook()
  boldstyle <- CellStyle(wb) + Font(wb, isBold = TRUE)
  currency <- CellStyle(wb) + DataFormat("$#,##0_);[Red]($#,##0)")
  comma <- CellStyle(wb) + DataFormat("#,##0.00")
  wholecomma <- CellStyle(wb) + DataFormat("#,##0")

  create.style.list <- function(style, startIndex, endIndex){
    toReturn = list()
    for (i in startIndex:endIndex){
      eval(parse(text = paste("toReturn$`",i,"`<-",style, sep = "")))
    }
    toReturn
  }

  yr.sheet <- createSheet(wb, sheetName = "Yearly Summary")
  yGrowthMetrics <- createCell(createRow(yr.sheet, rowIndex = 2), colIndex = 1)[[1,1]]
  setCellValue(yGrowthMetrics, "GrowthMetrics")
  setCellStyle(yGrowthMetrics, boldstyle)
  yProfitAndLoss <- createCell(createRow(yr.sheet, rowIndex = 6), colIndex = 1)[[1,1]]
  setCellValue(yProfitAndLoss, "ProfitAndLoss")
  setCellStyle(yProfitAndLoss, boldstyle)
  yCashFlow <- createCell(createRow(yr.sheet, rowIndex = 25), colIndex = 1)[[1,1]]
  setCellValue(yCashFlow, "CashFlow")
  setCellStyle(yCashFlow, boldstyle)
  yKeyRatios <- createCell(createRow(yr.sheet, rowIndex = 29), colIndex = 1)[[1,1]]
  setCellValue(yKeyRatios, "KeyRatios")
  setCellStyle(yKeyRatios, boldstyle)
  addDataFrame(data.frame(yrtemp[1:2,]), yr.sheet, col.names = TRUE, row.names = FALSE, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = append(list(`1` = boldstyle),
                                 create.style.list(style = "wholecomma", startIndex = 2, endIndex = ncol(yrtemp))))
  addDataFrame(data.frame(yrtemp[3:27,]), yr.sheet, col.names = FALSE, row.names = FALSE, startRow = 4, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = append(list(`1` = boldstyle),
                                 create.style.list(style = "currency", startIndex = 2, endIndex = ncol(yrtemp))))
  addDataFrame(data.frame(yrtemp[28:33,]), yr.sheet, col.names = FALSE, row.names = FALSE, startRow = 29, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = list(`1` = boldstyle))
  createFreezePane(yr.sheet, rowSplit = 2, colSplit = 3)
  autoSizeColumn(sheet=yr.sheet, colIndex = 1: (ncol(yrtemp)+1))

  mth.sheet <- createSheet(wb, sheetName = "Monthly Summary")
  mGrowthMetrics <- createCell(createRow(mth.sheet, rowIndex = 2), colIndex = 1)[[1,1]]
  setCellValue(mGrowthMetrics, "GrowthMetrics")
  setCellStyle(mGrowthMetrics, boldstyle)
  mProfitAndLoss <- createCell(createRow(mth.sheet, rowIndex = 10), colIndex = 1)[[1,1]]
  setCellValue(mProfitAndLoss, "ProfitAndLoss (before taxes)")
  setCellStyle(mProfitAndLoss, boldstyle)
  mCashFlow <- createCell(createRow(mth.sheet, rowIndex = 28), colIndex = 1)[[1,1]]
  setCellValue(mCashFlow, "CashFlow (before taxes)")
  setCellStyle(mCashFlow, boldstyle)

  addDataFrame(data.frame(mthdates), mth.sheet, col.names = TRUE, row.names = FALSE, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = list(`1` = boldstyle))
  addDataFrame(data.frame(mthtemp[1:4,]), mth.sheet, col.names = FALSE, row.names = FALSE, startRow = 3, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = append(list(`1` = boldstyle),
                                 create.style.list(style = "wholecomma", startIndex = 2, endIndex = ncol(mthtemp))))
  addDataFrame(data.frame(mthtemp[5:30,]), mth.sheet, col.names = FALSE, row.names = FALSE, startRow = 7, startColumn = 2, colnamesStyle = boldstyle,
               colStyle = append(list(`1` = boldstyle),
                                 create.style.list(style = "currency", startIndex = 2, endIndex = ncol(mthtemp))))
  createFreezePane(mth.sheet, rowSplit = 2, colSplit = 3)
  autoSizeColumn(sheet = mth.sheet, colIndex = 1:(ncol(mthtemp) +1))

  const.sheet <- createSheet(wb, sheetName = "Constants")
  addDataFrame(assumptions, const.sheet, col.names = TRUE, row.names = FALSE, colnamesStyle = boldstyle, colStyle = list(`2` = comma))
  createFreezePane(const.sheet, rowSplit = 2, colSplit = 1)
  autoSizeColumn(sheet = const.sheet, colIndex = 1:ncol(assumptions))

  irr <- data.frame(AnnualIRR=irr.month())
  irr$BackForEveryDollar <- lease.returns()$total.return
  irr.sheet <- createSheet(wb, sheetName = "LeaseIRR")
  addDataFrame(irr, irr.sheet, col.names = TRUE, row.names = FALSE, colnamesStyle = boldstyle, colStyle = list(`2` = comma))
  createFreezePane(irr.sheet, rowSplit = 2, colSplit = 1)
  autoSizeColumn(sheet = irr.sheet, colIndex = 1:ncol(irr))

  emp.sheet <- createSheet(wb, sheetName = "Employees")
  addDataFrame(emp.data, emp.sheet, col.names = TRUE, row.names = FALSE, colnamesStyle = boldstyle, colStyle = list(`3` = currency))
  createFreezePane(emp.sheet, rowSplit = 2, colSplit = 3)
  autoSizeColumn(sheet = emp.sheet, colIndex = 1:ncol(emp.data))

  growth.sheet <- createSheet(wb, sheetName = "Growth Projections")
  addDataFrame(growth.data, growth.sheet, col.names = TRUE, row.names = FALSE, colnamesStyle = boldstyle, colStyle = list(`2` = wholecomma))
  createFreezePane(growth.sheet, rowSplit = 2, colSplit = 1)
  autoSizeColumn(sheet = yr.sheet, colIndex = 1:ncol(growth.data))

  default.sheet <- createSheet(wb, sheetName = "Default Rate")
  cum.default.rate(file = "dtoAdd.png", small.text.size = 2, text.size = 8)
  addPicture(file = "dtoAdd.png", sheet = default.sheet)

  lease.cf.sheet <- createSheet(wb, sheetName = "Lease Cash Flow")
  cash.by.month(file = "cbmtoAdd.png",small.text.size = 2, text.size = 8)
  addPicture(file = "cbmtoAdd.png", sheet = lease.cf.sheet)

  bad.debt.sheet <- createSheet(wb, sheetName = "Bad Debt")
  bad.debt.to.pay(file = "bdptoAdd.png", small.text.size = 2, text.size = 8)
  addPicture(file = "bdptoAdd.png", sheet = bad.debt.sheet, startRow = 1)
  bad.debt.total.pdue(file = "bdtotalAdd.png", small.text.size = 2, text.size = 8)
  addPicture(file = "bdtotalAdd.png", sheet = bad.debt.sheet, startRow = 38)
  bad.debt.month(file = "bdtoAdd.png", small.text.size = 2, text.size = 8)
  addPicture(file = "bdtoAdd.png", sheet = bad.debt.sheet, startRow = 75)

  if (!is.null(file)){
   saveWorkbook(wb, file = file)
  }
  wb
}
