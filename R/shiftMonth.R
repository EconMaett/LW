# This function takes in a (year,month) date in time series format
# and a shift number, and returns the (year,month) date corresponding
# to the shift. Positive values of shift produce leads and negative values
# of shift produce lags.
# For example, entering 2014m1 with a shift of -1 would return 2013m12.
# Entering 2014m1 with a shift of 1 would return 2014m2.
# In each case, the first argument of the function must be entered as
# a two-element vector, where the first element corresponds to the year
# and the second element corresponds to the month.
# This function is analogous to shiftQuarter().
"shiftMonth" <- function(original.start, shift) {
  
  # Leads (positive values of shift)
  if (shift > 0) {
    new.start <- c(0, 0)
    sum <- original.start[2] + shift
    
    # Get the year value
    if (sum <= 12) {
      new.start[1] <- original.start[1]
    } else {
      new.start[1] <- original.start[1] + ceiling(sum / 12) - 1
    }
    
    # Get the month value
    if (sum %% 12 > 0) {
      new.start[2] <- sum %% 12
    } else {
      new.start[2] <- sum %% 12 + 12
    }
  }
  
  # Lags (negative values of shift)
  else {
    new.start <- c(0, 0)
    diff <- original.start[2] - abs(shift)
    
    # Get the year value
    if (diff > 0) {
      new.start[1] <- original.start[1]
    } else {
      new.start[1] <- original.start[1] - (1 + floor(abs(diff) / 12))
    }
    
    # Get the month value
    if (diff %% 12 > 0) {
      new.start[2] <- diff %% 12
    } else {
      new.start[2] <- diff %% 12 + 12
    }
  }
  
  return(new.start)
}

# END