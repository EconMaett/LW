# This function splices two series, with the series s2 beginning at splice.date
# and extended back using the growth rate at the splice.date times series s1
# The freq argument accepts two values - 'quarterly' and 'monthly' -
# but it could be modified to take more.
"splice" <- function(s1, s2, splice.date, freq) {
  t <- splice.date # renaming for convenience

  if (freq == "quarterly" | freq == "Quarterly") {
    t.minus.1 <- shiftQuarter(t, -1)
  } else if (freq == "monthly" | freq == "Monthly") {
    t.minus.1 <- shiftMonth(t, -1)
  } else {
    stop("You must enter 'quarterly' or 'monthly' for freq.")
  }

  ratio <- as.numeric(window(s2, start = t, end = t) / window(s1, start = t, end = t))

  return(mergeSeries(ratio * window(s1, end = t.minus.1), window(s2, start = t)))
}

# END