msproj.out <- function(res, res.list, res.list1, out.name, p.vars, country, area.vars) {
  det.out <- list()
  
  #-----1. Total population:-----
  if ("region" %in% p.vars) { #only region
    det.out$pop <- cbind(margin.table(res.list$pop, 1), margin.table(res.list$pop, c(1, 3)))
    colnames(det.out$pop)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #region + residence
      pop <- margin.table(res.list$pop, c(1, 3:4))
      pop.rur <- cbind((margin.table(res.list$pop, c(1, 4)))[, "rural"], pop[, , "rural"])
      pop.urb <- cbind((margin.table(res.list$pop, c(1, 4)))[, "urban"], pop[, , "urban"])                                 
      det.out$pop.res <- cbind(pop.rur, pop.urb)[, rep(1:ncol(pop.rur), each = 2) + rep(c(0, ncol(pop.rur)), ncol(pop.rur))]
      colnames(det.out$pop.res)[1:2] <- rep(country, 2)
      colnames(det.out$pop.res) <- paste(colnames(det.out$pop.res), c("rural", "urban"))
    }
  } else { #only total
    det.out$pop <- cbind(margin.table(res.list$pop, 1))
    colnames(det.out$pop)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #only residence
      det.out$pop.res <- cbind(margin.table(res.list$pop, c(1, 3)))
      colnames(det.out$pop.res) <- paste(country, c("rural", "urban"), sep = "_")       
      }
    }

  #-----2. Births:-----
  if ("region" %in% p.vars) { #only region
    det.out$births <- cbind(margin.table(res.list$births, 1), margin.table(res.list$births, c(1, 3)))
    colnames(det.out$births)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #region + residence
      births <- margin.table(res.list$births, c(1, 3:4))
      births.rur <- cbind((margin.table(res.list$births, c(1, 4)))[, "rural"], births[, , "rural"])
      births.urb <- cbind((margin.table(res.list$births, c(1, 4)))[, "urban"], births[, , "urban"])                                 
      det.out$births.res <- cbind(births.rur, births.urb)[, rep(1:ncol(births.rur), each = 2) + rep(c(0, ncol(births.rur)), ncol(births.rur))]
      colnames(det.out$births.res)[1:2] <- rep(country, 2)
      colnames(det.out$births.res) <- paste(colnames(det.out$births.res), c("rural", "urban"))
    }
  } else { #only total
    det.out$births <- cbind(margin.table(res.list$births, 1))
    colnames(det.out$births)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #only residence
      det.out$births.res <- cbind(margin.table(res.list$births, c(1, 3)))
      colnames(det.out$births.res) <- paste(country, c("rural", "urban"), sep = "_")       
    }
  }

  #-----3. Deaths:-----  
  if ("region" %in% p.vars) { #only region
    det.out$deaths <- cbind(margin.table(res.list$deaths, 1), margin.table(res.list$deaths, c(1, 3)))
    colnames(det.out$deaths)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #region + residence
      deaths <- margin.table(res.list$deaths, c(1, 3:4))
      deaths.rur <- cbind((margin.table(res.list$deaths, c(1, 4)))[, "rural"], deaths[, , "rural"])
      deaths.urb <- cbind((margin.table(res.list$deaths, c(1, 4)))[, "urban"], deaths[, , "urban"])                                 
      det.out$deaths.res <- cbind(deaths.rur, deaths.urb)[, rep(1:ncol(deaths.rur), each = 2) + rep(c(0, ncol(deaths.rur)), ncol(deaths.rur))]
      colnames(det.out$deaths.res)[1:2] <- rep(country, 2)
      colnames(det.out$deaths.res) <- paste(colnames(det.out$deaths.res), c("rural", "urban"))
    }
  } else { #only total
    det.out$deaths <- cbind(margin.table(res.list$deaths, 1))
    colnames(det.out$deaths)[1] <- country
    #by residence:
    if ("residence" %in% p.vars) { #only residence
      det.out$deaths.res <- cbind(margin.table(res.list$deaths, c(1, 3)))
      colnames(det.out$deaths.res) <- paste(country, c("rural", "urban"), sep = "_")       
    }
  }
  
  #-----4. Migration:-----
  if ("net.mig" %in% names(res.list1)) {
    det.out$inmig <- cbind(margin.table(res.list1$inmig, 1), margin.table(res.list1$inmig, c(1, 3)))
    colnames(det.out$inmig)[1] <- country
    det.out$outmig <- cbind(margin.table(res.list1$outmig, 1), margin.table(res.list1$outmig, c(1, 3)))
    colnames(det.out$outmig)[1] <- country
    }

  #-----5. TFR:----- 
  if ("region" %in% p.vars) {
    res.vars <- c("period", "region", "age")
    tfr <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
    for (j in seq_len(ncol(tfr)))
    set(tfr, which(is.na(tfr[[j]])), j, 0)
    det.out$tfr <- tfr[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")] 
  } else {
    res.vars <- c("period", "age")
    tfr <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
    for (j in seq_len(ncol(tfr)))
      set(tfr, which(is.na(tfr[[j]])), j, 0)
    det.out$tfr <- tfr[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]
    }
  
  res.vars <- c("period", "age")
  tfr.country <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
  for (j in seq_len(ncol(tfr.country)))
    set(tfr.country, which(is.na(tfr.country[[j]])), j, 0)
  det.out$tfr.country <- tfr.country[, .(TFR = round(sum(TFR) / 200, 2)), setdiff(res.vars, "age")]
  names(det.out)[match("tfr.country", names(det.out))] <- paste("tfr", country, sep = ".")
  
  if ("edu" %in% p.vars) {
    res.vars <- c("period", intersect(c("region", "age", "edu"), p.vars))
    tfr.edu <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
    for (j in seq_len(ncol(tfr.edu)))
      set(tfr.edu, which(is.na(tfr.edu[[j]])), j, 0)
    det.out$tfr.edu <- tfr.edu[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]
    }
  
  if ("residence" %in% p.vars) {
    if ("region" %in% p.vars) {
      res.vars <- c("period", "region", "age", "residence")
      tfr.res <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
      for (j in seq_len(ncol(tfr.res)))
        set(tfr.res, which(is.na(tfr.res[[j]])), j, 0)
      tfr.res <- tfr.res[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]    
      tfr.res$area <- apply(tfr.res[, area.vars, with = FALSE], 1, function(x) paste(x[1], x[2], sep = "_"))
      det.out$tfr.res <- tfr.res[, .(period, area, TFR)]
    } else {
    res.vars <- c("period", "residence", "age")
    tfr.res <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
    for (j in seq_len(ncol(tfr.res)))
      set(tfr.res, which(is.na(tfr.res[[j]])), j, 0)
    det.out$tfr.res <- tfr.res[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]    
    }
  }
 
  if (all(c("residence", "edu") %in% p.vars)) {
    if ("region" %in% p.vars) {
      res.vars <- c("period", "region", "age", "edu", "residence")
      tfr.edu.res <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
      for (j in seq_len(ncol(tfr.edu.res)))
        set(tfr.edu.res, which(is.na(tfr.edu.res[[j]])), j, 0)
      tfr.edu.res <- tfr.edu.res[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]    
      tfr.edu.res$area <- apply(tfr.edu.res[, area.vars, with = FALSE], 1, function(x) paste(x[1], x[2], sep = "_"))
      det.out$tfr.edu.res <- tfr.edu.res[ , .(period, area, edu, TFR)] 
    } else {
      res.vars <- c("period", "residence", "age", "edu")
      tfr.edu.res <- res[period < max(period) & age >= 15 & age < 50 & sex == "female", .(TFR = sum(births) / (sum(pop, pop3.shift) / 2) / 5 * 1000), by = res.vars]
      for (j in seq_len(ncol(tfr.edu.res)))
        set(tfr.edu.res, which(is.na(tfr.edu.res[[j]])), j, 0)
      det.out$tfr.edu.res <- tfr.edu.res[, .(TFR = round(sum(TFR) / 200, 2)), by = setdiff(res.vars, "age")]        
    }
  }

  #-----5. e4plus, e5plus:-----   
  #divide by the total number to get a percentage:
  if ("edu" %in% p.vars) {
    res.vars <- c("period", intersect(c("region"), p.vars), "sex")
    e4.plus <- res[age >= 15 & age < 35 & edu %in% c("e4", "e5", "e6"), .(perc = sum(pop)), by = res.vars]
    det.out$e4.plus <- e4.plus[, perc := round(perc / res[age >= 15 & age < 35, .(all.edu = sum(pop)), by = res.vars][, all.edu], 4) * 100]
  
    e5.plus <- res[age >= 25 & age < 35 & edu %in% c("e5", "e6") & sex == "female", .(perc = sum(pop)), by = setdiff(res.vars, "sex")]
    det.out$e5.plus <- e5.plus[, perc := round(perc / res[age >= 25 & age < 35 & sex == "female", 
                                                          .(all.edu = sum(pop)), by = setdiff(res.vars, "sex")][, all.edu], 4) * 100]
    }
  
  #reset option on exit
  op <- options(scipen = 0)
  options(scipen = 100)
  
  #csv's with numbers:    
  sapply(1:length(det.out), function(x) write.csv(det.out[[x]], paste(out.name, "_", names(det.out)[x], ".csv", sep = "")))
  
  #pdf's:
  if ("pop" %in% names(det.out)) {
    pdf(paste(out.name, "_pop.pdf", sep = ""))
    sapply(1:ncol(det.out$pop), function(x) plot(rownames(det.out$pop), det.out$pop[, x] / 1e6, type = "b", xlab = "year", 
                                                 ylab = "Total population in millions", main = c(colnames(det.out$pop)[x])))
    dev.off()
  }

  if ("pop.res" %in% names(det.out)) {  
    pdf(paste(out.name, "_pop_residence.pdf", sep = ""))
    sapply(1:ncol(det.out$pop.res), function(x) plot(rownames(det.out$pop.res), det.out$pop.res[, x] / 1e6, type = "b", xlab = "year", 
                                                     ylab = "Total population in millions", main = c(colnames(det.out$pop.res)[x])))
    dev.off()
    }
  
  if ("births" %in% names(det.out)) {
    pdf(paste(out.name, "_births.pdf", sep = ""))
    sapply(1:ncol(det.out$births), function(x) plot(rownames(det.out$births), det.out$births[, x] / 1e3, type = "b", xlab = "year", 
                                                    ylab = "Births in thousands", main = c(colnames(det.out$births)[x])))
    dev.off()
    }
  
  if ("births.res" %in% names(det.out)) {
    pdf(paste(out.name, "_births_residence.pdf", sep = ""))
    sapply(1:ncol(det.out$births.res), function(x) plot(rownames(det.out$births.res), det.out$births.res[, x] / 1e6, type = "b", xlab = "year", 
                                                        ylab = "Births in millions", main = c(colnames(det.out$births.res)[x])))
    dev.off()
    }
  
  if ("deaths" %in% names(det.out)) {
    pdf(paste(out.name, "_deaths.pdf", sep = ""))
    sapply(1:ncol(det.out$deaths), function(x) plot(rownames(det.out$deaths), det.out$deaths[, x] / 1e3, type = "b", xlab = "year", 
                                                    ylab = "Deaths in thousands", main = c(colnames(det.out$deaths)[x])))
    dev.off()
    }
  
  if ("deaths.res" %in% names(det.out)) {
    pdf(paste(out.name, "_deaths_residence.pdf", sep = ""))
    sapply(1:ncol(det.out$deaths.res), function(x) plot(rownames(det.out$deaths.res), det.out$deaths.res[, x] / 1e6, type = "b", xlab = "year", 
                                                        ylab = "Deaths in millions", main = c(colnames(det.out$deaths.res)[x])))
    dev.off()
    }
  
  if (any(c("inmig", "outmig") %in% names(det.out))) {
    pdf(paste(out.name, "_mig.pdf", sep = ""))
    sapply(1:ncol(det.out$inmig), function(x) {
      limits <- apply(rbind(det.out$inmig, det.out$outmig), 2, range)
      limits[2, ] <- limits[2, ] * 1.1
      plot(rownames(det.out$inmig), det.out$inmig[, x] / 1e3, type = "b", xlab = "year", ylim = sort(limits[, x] / 1e3),
           ylab = "Internal migration in thousands",
           main = c(colnames(det.out$outmig)[x]))
      lines(rownames(det.out$outmig), det.out$outmig[, x] / 1e3, type = "b", col = 2)
      legend("topright", c("In-migration", "Out-migration"), lty = 1, pch = 1, col = 1:2)
      })
    dev.off()
    }

  if ("tfr.edu" %in% names(det.out) & "region" %in% p.vars) {  
    pdf(paste(out.name, "_tfr_edu.pdf", sep = ""))
    #if ("region" %in% p.vars) {
      sapply(unique(det.out$tfr.edu$region), function(x) {
        tfr.edu.x <- det.out$tfr.edu[det.out$tfr.edu$region == x]
        tfr.x <- det.out$tfr[det.out$tfr$region == x]
        limits <- range(tfr.edu.x$TFR)
        limits[2] <- limits[2] * 1.2
        matplot(t(matrix(tfr.edu.x$period, nrow = 6)), t(matrix(tfr.edu.x$TFR, nrow = 6)), type = "b", lty = 1, pch = 1:6, col = 1, main = x, xlab = "time", 
                ylab = "TFR", ylim = limits)
        with(tfr.x, lines(period, TFR, lwd = 1.5, type = "b", col = "blue", pch = 16))
        legend("topright", c(paste("TFR e", 1:6, sep = ""), "TFR overall"), col = c(rep(1, 6), "blue"), pch = c(1:6, 16))
      }) 
    # } else {
    #   limits <- range(det.out$tfr.edu$TFR)
    #   limits[2] <- limits[2] * 1.2
    #   matplot(t(matrix(det.out$tfr.edu$period, nrow = 6)), t(matrix(det.out$tfr.edu$TFR, nrow = 6)), type = "b", lty = 1, pch = 1:6, col = 1, main = x, xlab = "time", 
    #           ylab = "TFR", ylim = limits)
    #   with(det.out$tfr, lines(period, TFR, lwd = 1.5, type = "b", col = "blue", pch = 16))
    #   legend("topright", c(paste("TFR e", 1:6, sep = ""), "TFR overall"), col = c(rep(1, 6), "blue"), pch = c(1:6, 16))
    #     })
    dev.off() 
    }

  if ("tfr.edu.res" %in% names(det.out) & "region" %in% p.vars) { 
    pdf(paste(out.name, "_tfr_edu_residence.pdf", sep = ""))
    #if ("region" %in% p.vars) {
      sapply(unique(det.out$tfr.edu.res$area), function(x) {
        tfr.edu.res.x <- det.out$tfr.edu.res[det.out$tfr.edu.res$area == x] #area has to be used here instead of region!
        tfr.edu.res.x$TFR[tfr.edu.res.x$TFR == Inf] <- 0 #MW 2016-08-05 - prevent infinite values
        tfr.res.x <- det.out$tfr.res[det.out$tfr.res$area == x]
        limits <- range(tfr.edu.res.x$TFR)
        limits[2] <- limits[2] * 1.2
        matplot(t(matrix(tfr.edu.res.x$period, nrow = 6)), t(matrix(tfr.edu.res.x$TFR, nrow = 6)), type = "b", lty = 1, pch = 1:6, col = 1, main = x,
                xlab = "time", ylab = "TFR", ylim = limits)
        with(tfr.res.x, lines(period, TFR, lwd = 1.5, type = "b", col = "blue", pch = 16))
        legend("topright", c(paste("TFR e", 1:6, sep = ""), "TFR overall"), col = c(rep(1, 6), "blue"), pch = c(1:6, 16))
        })
    # } else {
    #   limits <- range(det.out$tfr.edu.res$TFR)
    #   limits[2] <- limits[2] * 1.2
    #   matplot(t(matrix(det.out$tfr.edu.res$period, nrow = 6)), t(matrix(det.out$tfr.edu.res$TFR, nrow = 6)), type = "b", lty = 1, pch = 1:6, col = 1, main = x, xlab = "time", 
    #           ylab = "TFR", ylim = limits)
    #   with(det.out$tfr, lines(period, TFR, lwd = 1.5, type = "b", col = "blue", pch = 16))
    #   legend("topright", c(paste("TFR e", 1:6, sep = ""), "TFR overall"), col = c(rep(1, 6), "blue"), pch = c(1:6, 16))
    # })
    dev.off() 
  }

  if ("e4.plus" %in% names(det.out)) {  
    pdf(paste(out.name, "_e4_plus.pdf", sep = ""))
    if ("region" %in% p.vars) {
      sapply(unique(det.out$e4.plus$region), function(x) {
      e4.plus.x <- det.out$e4.plus[det.out$e4.plus$region == x]
      matplot(t(matrix(e4.plus.x$period, nrow = 2)), t(matrix(e4.plus.x$perc, nrow = 2)), type = "b", pch = 1, lty = 1, col = 1:2, main = x, 
              xlab = "time", ylab = "Percentage e4+", ylim = c(0, 100))
      legend("topleft", c("female", "male"), lty = 1, pch = 1, col = 1:2)
      })
    } else {
      matplot(t(matrix(det.out$e4.plus$period, nrow = 2)), t(matrix(det.out$e4.plus$perc, nrow = 2)), type = "b", lty = 1, pch = 1:6, col = 1, 
              main = country, xlab = "time", ylab = "Percentage e4+")
      legend("topleft", c("female", "male"), lty = 1, pch = 1:2)
      }
    dev.off() 
  }
  
  if ("e5.plus" %in% names(det.out)) {  
    pdf(paste(out.name, "_e5_plus.pdf", sep = ""))
    if ("region" %in% p.vars) {
      sapply(unique(det.out$e5.plus$region), function(x) {
        e5.plus.x <- det.out$e5.plus[det.out$e5.plus$region == x]
        with(e5.plus.x, plot(period, perc, type = "b", lty = 1, main = x, xlab = "time", ylab = "Percentage e5+", ylim = c(0, 100)))
        legend("topleft", c("female"), col = 1, lty = 1, pch = 1)
        })
    } else {
      with(det.out$e5.plus, plot(period, perc, type = "b", lty = 1, main = country, xlab = "time", ylab = "Percentage e5+", ylim = c(0, 100)))
      legend("topleft", c("female"), col = 1, lty = 1, pch = 1)
      }
    dev.off() 
  }

  options(op)
}