#---------------------------------------------------------------------------------------#
# File: kalman.states.R
# Description: The function kalman.states() calls the functions kalman.states.filtered() 
# and kalman.states.smoothed() to apply the Kalman filter and smoother.
# It takes as input the coefficient matrices for the given state-space model, 
# with notation matching Hamilton (1994), as well as conditional expectation and 
# covariance matrix of the initial state, xi.tm1tm1 and P.tm1tm1 respectively.
#---------------------------------------------------------------------------------------#
"kalman.states" <- function(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, cons, y, x) {
  filtered <- kalman.states.filtered(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, cons, y, x)
  smoothed <- kalman.states.smoothed(
    filtered$xi.ttm1, filtered$P.ttm1, filtered$xi.tt, filtered$P.tt,
    F, Q, A, H, R, kappa, y, x
  )
  return(list("filtered" = filtered, "smoothed" = smoothed))
}

# END