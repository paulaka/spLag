Lag.Matrix <-
function (input, W, time, obs, vartime, varid, asymmetriclow = FALSE,
    asymmetrichigh = FALSE, sizevar, size){
    h <- input
    h <- reshape(h, idvar = varid, timevar = vartime,
        direction = "wide")
    for (i in 2:(time + 1)) {
        h[, i] <- W %*% h[, i]
    }
    h <- reshape(h, idvar = varid, direction = "long", timevar = vartime)
    h2 <- h[, 3]
    if (asymmetriclow == TRUE) {
      sizevar <- sizevar
        h2 <- ifelse(sizevar < size, h2, 0)
    }
    if (asymmetrichigh == TRUE) {
      sizevar <- sizevar
        h2 <- ifelse(sizevar > size, h2, 0)
    }
    start <- as.vector(rep(NA, times = obs))
    lag <- append(start, h2[1:(length(h2) - obs)])
    return(lag)
}
