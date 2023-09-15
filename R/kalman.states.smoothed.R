"kalman.states.smoothed" <- function(xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
                                     F, Q, A, H, R, kappa, y, x, t.i = dim(y)[1], xi.tp1T = NA, P.tp1T = NA) {
  n <- dim(xi.ttm1.array)[2]
  if (t.i == dim(y)[1]) {
    xi.tT <- xi.tt.array[t.i, ]
    P.tT <- P.tt.array[((t.i - 1) * n + 1):(t.i * n), ]
    tmp <- kalman.states.smoothed(
      xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
      F, Q, A, H, R, kappa, y, x, t.i - 1, xi.tT, P.tT
    )
    return(list(
      "xi.tT" = rbind(tmp$xi.tT, xi.tT),
      "P.tT" = rbind(tmp$P.tT, P.tT)
    ))
  } else {
    P.tt <- P.tt.array[((t.i - 1) * n + 1):(t.i * n), ]
    P.tp1t <- P.ttm1.array[(t.i * n + 1):((t.i + 1) * n), ]
    J.t <- P.tt %*% t(F) %*% solve(P.tp1t)
    xi.tt <- xi.tt.array[t.i, ]
    xi.tp1t <- xi.ttm1.array[t.i + 1, ]
    xi.tT <- xi.tt + as.vector(J.t %*% (xi.tp1T - xi.tp1t))
    P.tT <- P.tt + J.t %*% (P.tp1T - P.tp1t) %*% t(J.t)
    if (t.i > 1) {
      tmp <- kalman.states.smoothed(
        xi.ttm1.array, P.ttm1.array, xi.tt.array, P.tt.array,
        F, Q, A, H, R, kappa, y, x, t.i - 1, xi.tT, P.tT
      )
      return(list(
        "xi.tT" = rbind(tmp$xi.tT, xi.tT),
        "P.tT" = rbind(tmp$P.tT, P.tT)
      ))
    } else {
      return(list("xi.tT" = xi.tT, "P.tT" = P.tT))
    }
  }
}

# END