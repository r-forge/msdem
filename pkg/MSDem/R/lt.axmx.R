lt.axmx <- function(axmx.dt) {
  axmx <- arrange(axmx.dt,age)
  mx <- subset(axmx,var=="mx")$value
  ax <- subset(axmx,var=="ax")$value
  ages <- subset(axmx,var=="ax")$age
  nage <- length(mx)
  lx <- dx <- Lx <- Tx <- ex <- rep(0,nage)
  nx <- c(1, 4, rep(5, nage-2))
  lx[1] <- 100000
  for(i in 2:nage) {
    if (max(ax) < 1) {
      lx[i] <- lx[i-1] * (1-mx[i-1]*nx[i-1]*ax[i-1])/(1+mx[i-1]*nx[i-1]*(1-ax[i-1]))
      dx[i-1] <- lx[i-1]-lx[i]
      Lx[i-1] <- nx[i-1]*lx[i]+nx[i-1]*ax[i-1]* dx[i-1]
    } else {
      lx[i] <- lx[i-1] * (1-mx[i-1]*ax[i-1])/(1+mx[i-1]*(nx[i-1]-ax[i-1]))
      dx[i-1] <- lx[i-1]-lx[i]
      Lx[i-1] <- nx[i-1]*lx[i]+ax[i-1]* dx[i-1]     
    }
  }
  Lx[i] <- lx[i]/mx[i]
  for(i in nage:1) Tx[i] <- sum(Lx[i:nage])
  ex <- Tx/lx
  nsx <- c(sum(Lx[1:2]) / 500000,#to age 0-4
             Lx[3] / sum(Lx[1:2]), #to age 5-9
             Lx[4:nage] / Lx[3:(nage-1)], #to age 10-14 ... 100+
             Lx[nage] / sum(Lx[(nage-1):nage]) #survival of the 100+  
  )
  ltx <- cbind(age = seq(0, 105, by = 5), nsx)
  ltx  
}
