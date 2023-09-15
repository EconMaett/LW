"kalman.states.filtered" <- function(xi.tm1tm1, P.tm1tm1, F, Q, A, H, R, kappa, cons, y, x, t.i = 1) {
  xi.ttm1 <- as.vector(F %*% xi.tm1tm1 + cons)
  P.ttm1 <- F %*% P.tm1tm1 %*% t(F) + Q
  prediction.error <- (as.vector(y[t.i, ]) - as.vector(t(A) %*% as.vector(x[t.i, ])) - as.vector(t(H) %*% xi.ttm1))
  HPHR <- t(H) %*% P.ttm1 %*% H + (kappa[t.i]^2) * R
  xi.tt <- xi.ttm1 + as.vector(P.ttm1 %*% H %*% solve(HPHR, prediction.error))
  P.tt <- P.ttm1 - P.ttm1 %*% H %*% solve(HPHR, t(H) %*% P.ttm1)
  kalman.gain <- P.ttm1 %*% H %*% solve(HPHR) # dim(xi.tt) x 2 matrix
  if (t.i == dim(y)[1]) {
    return(list(
      "xi.ttm1" = xi.ttm1, "P.ttm1" = P.ttm1, "xi.tt" = xi.tt, "P.tt" = P.tt,
      "prediction.error" = prediction.error, "kalman.gain" = kalman.gain
    ))
  } else {
    tmp <- kalman.states.filtered(xi.tt, P.tt, F, Q, A, H, R, kappa, cons, y, x, t.i + 1)
    return(list(
      "xi.ttm1" = rbind(xi.ttm1, tmp$xi.ttm1),
      "P.ttm1" = rbind(P.ttm1, tmp$P.ttm1),
      "xi.tt" = rbind(xi.tt, tmp$xi.tt),
      "P.tt" = rbind(P.tt, tmp$P.tt),
      "prediction.error" = rbind(prediction.error, tmp$prediction.error),
      "kalman.gain" = rbind(kalman.gain, tmp$kalman.gain)
    ))
  }
}

# END