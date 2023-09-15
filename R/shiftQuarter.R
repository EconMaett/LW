# This function takes in a (year,quarter) date in time series format
# and a shift number, and returns the (year,quarter) date corresponding
# to the shift. Positive values of shift produce leads and negative values
# of shift produce lags.
# For example, entering 2014q1 with a shift of -1 would return 2013q4.
# Entering 2014q1 with a shift of 1 would return 2014q2.
# In each case, the first argument of the function must be entered as
# a two-element vector, where the first element corresponds to the year
# and the second element corresponds to the quarter.
# For example, Q12014 must be entered as "c(2014,1)".
"shiftQuarter" <- function(original.start, shift) {
  
  # Leads (positive values of shift)
  if (shift > 0) {
    new.start <- c(0, 0)
    sum <- original.start[2] + shift
    
    # Get the year value
    if (sum <= 4) {
      new.start[1] <- original.start[1]
    } else {
      new.start[1] <- original.start[1] + ceiling(sum / 4) - 1
    }
    
    # Get the quarter value
    if (sum %% 4 > 0) {
      new.start[2] <- sum %% 4
    } else {
      new.start[2] <- sum %% 4 + 4
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
      new.start[1] <- original.start[1] - (1 + floor(abs(diff) / 4))
    }
    
    # Get the quarter value
    if (diff %% 4 > 0) {
      new.start[2] <- diff %% 4
    } else {
      new.start[2] <- diff %% 4 + 4
    }
  }
  
  return(new.start)
}

# END