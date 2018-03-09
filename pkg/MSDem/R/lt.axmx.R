lt.axmx <- function(axmx.dt) {
  axmx <- arrange(axmx.dt, age)
  mx <- subset(axmx, var == "mx")$value
  ax <- subset(axmx, var == "ax")$value
  nage <- length(mx)
  nx <- c(1, 4, rep(5, nage - 2))
  tol <- 1e-5
  if (max(ax) < 1) {
    ax.probl <- (nx * ax) - (1 / mx) > tol
    if(any(ax.probl)) {
      ax[ax.probl] <- 1 / (mx[ax.probl] * nx[ax.probl]) 
      warning("ax has been adjusted to match mx values")
      }
  } else {
    ax.probl <- ax - (1 / mx) > tol
    if(any(ax.probl)) {
      ax[ax.probl] <- 1 / (mx[ax.probl]) 
      warning("ax has been adjusted to match mx values")
    }
  }
  
  ages <- subset(axmx, var == "ax")$age
  lx <- dx <- Lx <- Tx <- ex <- rep(0, nage)
  lx[1] <- 100000
  for (i in 2:nage) {
    if (max(ax) < 1) {
      lx[i] <- lx[i - 1] * (1 - mx[i - 1] * nx[i - 1] * ax[i - 1]) / (1 + mx[i - 1] * nx[i - 1] * (1 - ax[i - 1]))
      dx[i - 1] <- lx[i - 1] - lx[i]
      Lx[i - 1] <- nx[i - 1] * lx[i] + nx[i - 1] * ax[i - 1] * dx[i - 1]
    } else {
      lx[i] <- lx[i - 1] * (1 - mx[i - 1] * ax[i - 1]) / (1 + mx[i - 1] * (nx[i - 1] - ax[i - 1]))
      dx[i - 1] <- lx[i - 1] - lx[i]
      Lx[i - 1] <- nx[i - 1] * lx[i] + ax[i - 1] * dx[i - 1]     
    }
  }
  Lx[i] <- lx[i] * ax[i]
  for(i in nage:1) Tx[i] <- sum(Lx[i:nage])
  ex <- Tx / lx
  nSx <- c(sum(Lx[1:2]) / 500000,#to age 0-4
             Lx[3] / sum(Lx[1:2]), #to age 5-9
             Lx[4:nage] / Lx[3:(nage - 1)], #to age 10-14 ... 100+
             Lx[nage] / sum(Lx[(nage - 1):nage]) #survival of the 100+  
  )
  ltx <- cbind(age = unique(axmx.dt$age), nSx)
}
