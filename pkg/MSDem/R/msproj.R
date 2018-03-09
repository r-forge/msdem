msproj <- function(data = NULL, patt = NULL, country = NULL, SSP = "SSP2", fert = NULL, mort = NULL, int.mig = NULL, recl = NULL, edu = NULL, 
                   detail.out = FALSE, nSx = "axmx", iPr = 18, reclass.model = NULL, reclass.period = 5, 
                   maxeapr5 = c(urban = 0.7, rural = 0.5, total = 0.6)) {
  "%nin%" <- function(x, y) !x %in% y
  is.not.null <- function(x) !is.null(x)
  
  #positions of data files in the list:
  pos.st.sp <- grep("state.space", names(data))
  pos.mig <- grep("mig", names(data))
  pos.var.def <- grep("var.def", names(data))
  pos.axmx <- grep("axmx", names(data))
  spatial.vars <- intersect(c("region", "residence"), data[[pos.var.def]]$variables)
  
  if (length(country) == 0) {
    country <- as.character(data[[pos.var.def]][variables == "country", values])
    #if there is still no country, set "country" as country:
    if (length(country) == 0) country <- "country"
    }
  
  #-----sanity checks-----

  #check for duplicate column names:
  dups.idx <- which(sapply(data, function(x) any(duplicated(names(x)))))
  dups <- sapply(c("axmx", "mig", "state_space", "var_def"), function(x) grep(x, names(dups.idx)))
  names(dups) <- c("axmx", "migration", "state space", "variable definition")
  dups <- names(unlist(dups))
  
  if (length(dups) > 0) {
    stop(paste(rep("There are duplicate column names in your", length(dups)), dups, "file. "))
  }
  
  #check if there is any NA in the data sets:
  check.na <- function(x) { 
    if (any(names(x) == "mrate")) {
      data.cols <- which(names(x) == "mrate")
    } else {
      data.cols <- names(x)[-c(1:which(names(x) == "var"))]
    }
    which(!complete.cases(x[, data.cols, with = FALSE]))
  }
  x <- check.na(data[[pos.st.sp]])
  if (length(pos.mig > 0)) {
    y <- check.na(data[[pos.mig]])
  } else {
    y <- NULL
  }

  if (length(x) > 0 | length(y) > 0) {
    x.stop <- y.stop <- NULL
    if (length(x) > 0) {
      x.stop <- paste("Please check row(s)", paste(x, collapse = ", "), "in your state space file.")
    }
    if (length(y) > 0) {
      y.stop <- paste("Please check row(s)", paste(y, collapse = ", "), "in your migration file.")
    }    
    stop(paste("There are missing values in your data set(s).", x.stop, y.stop))
    }
  
  #check for columns - rows shouldn't be needed - that are empty (!= check for NAs):
  check.empty <- function(x) {
    which(sapply(x, function(x) all(x == "")))
    }
  
  x <- check.empty(data[[pos.st.sp]])
  if (length(pos.mig > 0)) {
    y <- check.empty(data[[pos.mig]])
  } else {
    y <- NULL
  }

  if (length(x) > 0 | length(y) > 0) {
    x.stop <- y.stop <- NULL
    if (length(x) > 0) {
      x.stop <- paste("Please check column(s)", paste(x, collapse = ", "), "in your state space file.")
    }
    if (length(y) > 0) {
      y.stop <- paste("Please check column(s)", paste(y, collapse = ", "), "in your migration file.")
    }    
    stop(paste("There are empty columns in your data set(s).", x.stop, y.stop))
  }
         
  #1. reclass:
  reclass.dt <- data[[pos.st.sp]][var %in% c("reclasstr", "gap", "perural")]
  if (nrow(reclass.dt) > 0) {
    m.vars <- names(reclass.dt)[-c(1:which(names(reclass.dt) == "var"))]
    reclass.dt <- melt(reclass.dt, measure.vars = m.vars)
    reclass.dt <- reclass.dt[grep("rural", reclass.dt$variable), ]
    reclass.dt <- dcast(reclass.dt, factor(variable) ~ var, value.var = "value")
    names(reclass.dt)[1] <- "area"
    reclass.dt$area <- sapply(reclass.dt$area, function(x) sub("_rural", "", x))
    areas.oldtr <- reclass.dt[reclasstr > 0.05 & perural > 0.4 | sapply(reclass.dt$reclasstr, function(x) identical(all.equal(x, 0), TRUE)), 
                              .(area = area, reclasstr)]
    if (any(reclass.dt$reclasstr > 0) & is.not.null(reclass.model)) {
      load(paste("input_models/", reclass.model, sep = ""))
      }
    } else {
    rm(reclass.dt)
  }
  
  #2. sexratio:
  sexratio.dt <- data[[pos.st.sp]][var == "sexr"]
  if (nrow(sexratio.dt) == 0) {
    sexratio.dt <- data[[pos.st.sp]][var %in% c("sexr_rur", "sexr_urb", "sexr_tot")]
    }
  sexratio.dt <- melt(sexratio.dt, id.vars = c("var"), measure.vars = names(sexratio.dt)[-c(1:which(names(sexratio.dt) == "var"))], 
                      variable.name = "area", value.name = "sexr")
  sexratio.dt <- sexratio.dt[sexr != 0][, var := NULL]
  if (any(sexratio.dt$area == "value")) sexratio.dt$area <- country
  
  #column order for popedu.dt, asfredu.dt, ass.edu.dt, inter.dt and nsxedu.dt:
  col.order <- c("period", "sex", "age", "edu", "region", "residence", "origin", "destination", "pattern", "var", "value")
  #3. popedu:
  popedu.dt <- data[[pos.st.sp]][var == "pop"]
  if (any(c("region", "residence") %in% spatial.vars)) {
    id.vars <- intersect(c("period", "sex", "age", "edu", "var"), names(popedu.dt))
    m.vars <- names(popedu.dt)[-c(1:which(names(popedu.dt) == "var"))]
    popedu.dt <- melt(popedu.dt, id.vars = id.vars, measure.vars = m.vars)
    if (all(c("region", "residence") %in% spatial.vars)) { #region and residence
      popedu.dt <- popedu.dt[, `:=` (region = factor(gsub("_.*", "", variable)), residence = factor(gsub(".*_", "", variable)), variable = NULL)]
    } else if ("residence" %in% spatial.vars) { #only residence
      setnames(popedu.dt, "variable", "residence") 
    } else if ("region" %in% spatial.vars) { #only region
      setnames(popedu.dt, "variable", "region")
    } 
  }
  p.vars <- names(popedu.dt)[names(popedu.dt) %in% c("sex", "age", "edu", "region", "residence")]
  popedu.dt[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
  setcolorder(popedu.dt, col.order[col.order %in% names(popedu.dt)])
 
  #If an area (=region/residence combination) has inhabitants of a certain age, 
  #there must be males and females - check for a positive number of inhabitants first, 
  #then check if this is true for both sexes:
  # mf.miss <-  sapply(seq(0, 30, 5), function(x) {
  #   mf.pos <- popedu.dt[age == x, .(Freq = sum(value)), by = c("region", "residence", "sex")]
  #   mf.pos.ftab <- ftable(xtabs(Freq ~ region + sex + residence, data = mf.pos), row.vars = c(1, 3))
  #   idx.F <- apply(matrix(mf.pos.ftab, ncol = 2), 1, function(x) xor(all(x == 0), all(x > 0)))
  #   areas.F <- expand.grid(attr(mf.pos.ftab, "row.vars")$residence, attr(mf.pos.ftab, "row.vars")$region)[2:1][idx.F == FALSE, ]
  #   }, simplify = FALSE)
  # names(mf.miss) <- seq(0, 30, 5)
  # if (any(sapply(mf.miss, nrow) > 0)) {
  #   stop(c("There are either no male or female inhabitants of age ", paste(names(mf.miss[sapply(mf.miss, nrow) > 0]), collapse = ", "), " in some of the areas."))
  # }
  #4. nsxedu:
  if (nSx == "axmx") {
    if (length(pos.axmx) > 0) {
      nsxedu.dt <- data[[pos.axmx]]
    } else {
      axmx.dt <- data[[pos.st.sp]][var %in% c("ax", "mx")]
      if (any(c("region", "residence") %in% spatial.vars)) {
        id.vars <- intersect(c("period", "sex", "age", "edu", "var"), names(axmx.dt))
        m.vars <- names(axmx.dt)[-c(1:which(names(axmx.dt) == "var"))]
        axmx.dt <- melt(axmx.dt, id.vars = id.vars, measure.vars = m.vars)
        if (all(c("region", "residence") %in% spatial.vars)) { #region and residence
          axmx.dt <- axmx.dt[, `:=` (region = factor(gsub("_.*", "", variable)), residence = factor(gsub(".*_", "", variable)), variable = NULL)]
        } else if ("residence" %in% spatial.vars) { #only residence
          setnames(axmx.dt, "variable", "residence")
        } else if ("region" %in% spatial.vars) { #only region
          setnames(axmx.dt, "variable", "region")
        }
      }
    axmx.dt[, axmxvars := do.call(paste, c(.SD, sep = "_")), .SDcols = c("period", setdiff(p.vars, "age"))]
    nsxedu.dt <- data.table(ddply(axmx.dt, .(axmxvars), lt.axmx))
    nsxedu.split <- tstrsplit(nsxedu.dt$axmxvars, split = "_")
    nsxedu.dt$period <- nsxedu.split[1]
    if (length(nsxedu.split) > 2) {
      patt.temp <- data.table(nsxedu.split[[2]], nsxedu.dt$age - 5, matrix(unlist(nsxedu.split[-(1:2)]), nrow = nrow(nsxedu.dt)))
    } else {
      patt.temp <- data.table(nsxedu.split[[2]], nsxedu.dt$age - 5)
    }
    nsxedu.dt$pattern <- do.call(paste, c(patt.temp, sep = "_"))
    nsxedu.dt[, `:=` (axmxvars = NULL, age = NULL)]
    setcolorder(nsxedu.dt, c("period", "pattern", "nSx"))
    #write.csv(nsxedu.dt, paste("input_data/", patt, "_axmx.csv", sep = ""), row.names = FALSE)
    }
  #sanity check:
  if (any(nsxedu.dt$nSx < 0 | nsxedu.dt$nSx > 1)) {
    stop(paste("nSx-values smaller than 0 or larger than 1 occured.\n Please check the values for ax and mx in your input data."))#, e.g., in lines",  
    #                 paste(which(nsxedu.dt$nSx < 0 | nsxedu.dt$nSx > 1)[1:10], collapse = ", "), "etc."))
    # nsxedu.dt[nsxedu.dt$nSx < 0 | nsxedu.dt$nSx > 1]
    }
  }
  #5. asfredu:
  asfredu.dt <- data[[pos.st.sp]][var == "asfr"]
  if (any(c("region", "residence") %in% spatial.vars)) {
    id.vars <- intersect(c("period", "sex", "age", "edu", "var"), names(asfredu.dt))
    m.vars <- names(asfredu.dt)[-c(1:which(names(asfredu.dt) == "var"))]
    asfredu.dt <- melt(asfredu.dt, id.vars = id.vars, measure.vars = m.vars)
    if (all(c("region", "residence") %in% spatial.vars)) { #region and residence
      asfredu.dt <- asfredu.dt[, `:=` (region = factor(gsub("_.*", "", variable)), residence = factor(gsub(".*_", "", variable)), variable = NULL)]
    } else if ("residence" %in% spatial.vars) { #only residence
      setnames(asfredu.dt, "variable", "residence") 
    } else if ("region" %in% spatial.vars) { #only region
      setnames(asfredu.dt, "variable", "region")
    } 
  }
  asfredu.dt[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
  asfredu.dt <- asfredu.dt[, .(period, pattern, value)]
  setnames(asfredu.dt, "value", "asfr")
  #6. ass.edu:
  ass.edu.dt <- data[[pos.st.sp]][var == "eapr"]
  #sanity check:
#   tab.eaprs <- table(ass.edu.dt$edu, ass.edu.dt$age)
#   if (any(apply(tab.eaprs, 1, function(x) sum(x > 0)) > 1)) {
#     stop(paste("Each possible educational transition should only have one eapr assigned to it, namely the one for the maximum age it can happen.\n 
# In the data, some transitions are associated with more than one age level. Please check your input data."))
#   }
    if (nrow(ass.edu.dt) > 0) {
    m.vars <- names(ass.edu.dt)[-c(1:which(names(ass.edu.dt) == "var"))]
    areasex.vars <- c("region", "residence", "sex")
    idx <- sapply(rowSums(ass.edu.dt[, (m.vars), with = FALSE]), function(x) identical(all.equal(x, 0), TRUE))
    ass.edu.dt <- subset(ass.edu.dt, !idx)
    if (any(c("region", "residence") %in% spatial.vars)) {
      id.vars <- intersect(c("period", "sex", "age", "edu", "var"), names(ass.edu.dt))
      ass.edu.dt <- melt(ass.edu.dt, id.vars = id.vars, measure.vars = m.vars)
      if (all(c("region", "residence") %in% spatial.vars)) { #region and residence
        ass.edu.dt <- ass.edu.dt[, `:=` (region = factor(gsub("_.*", "", variable)), residence = factor(gsub(".*_", "", variable)), variable = NULL)]
      } else if ("residence" %in% spatial.vars) { #only residence
        setnames(ass.edu.dt, "variable", "residence") 
      } else if ("region" %in% spatial.vars) { #only region
        setnames(ass.edu.dt, "variable", "region")
      } 
    }
    ass.edu.dt[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
    setcolorder(ass.edu.dt, col.order[col.order %in% names(ass.edu.dt)])
    ass.edu.dt[, areasex := do.call(paste, c(.SD, sep = "_")), .SDcols = intersect(names(ass.edu.dt), areasex.vars)]
  } else {
    ass.edu.dt <- NULL
    }
  #7. inter:
  if (length(pos.mig) > 0) {
    inter.dt <- data[[pos.mig]]
    #MW 2017-03-28: delete period information, otherwise matching projedu and inter is not possible from 2015 on
    if ("period" %in% names(inter.dt)) inter.dt[, period := NULL]
    if (any(names(inter.dt) == "mrate")) {
      setnames(inter.dt, "mrate", "value")
    } else {
      data.cols <- names(inter.dt)[-c(1:which(names(inter.dt) == "var"))]
      typeof.tab <- table(sapply(inter.dt[, .SD, .SDcols = data.cols], typeof)) #to prevent warning message
      mode(inter.dt$World) <- names(which(typeof.tab == max(typeof.tab)))
      inter.dt <- melt(inter.dt, measure.vars = data.cols, variable.name = "destination")
    }
    inter.dt[, age := age - 5]
    inter.dt <- inter.dt[age >= 0]
    setcolorder(inter.dt, col.order[col.order %in% names(inter.dt)])
    }

  migr.contrl <- 1
  if (exists("inter.dt")) inter.dt[, value := value * migr.contrl]
  
  #-----------data preparation:----------

  SSP.name <- SSP
  
  SSP1 <- list(fert = c(seq(0.95, 0.8, -0.05), seq(0.7875, 0.75, -0.0125), rep(0.75, 10)), 
               mort = 0.5, 
               int.mig = c(seq(1.125, 1.5, 0.125), rep(1.5, 14)), 
               recl = rep(1, 18), 
               edu = 1)
  SSP2 <- list(fert = 1, 
               mort = 0, 
               int.mig = rep(1, 18), 
               recl = rep(1, 18), 
               edu = 1)
  SSP3 <- list(fert = c(seq(1.05, 1.2, 0.05), seq(1.2125, 1.25, 0.0125), rep(1.25, 10)), 
               mort = -0.5, 
               int.mig = c(seq(0.875, 0.5, -0.125), rep(0.5, 14)), 
               recl = c(seq(1.125, 1.5, 0.125), rep(1.5, 14)), 
               edu = 1)
  SSP4 <- list(fert = c(seq(0.95, 0.8, -0.05), seq(0.7875, 0.75, -0.0125), rep(0.75, 10)), #MW 2016-08-31: changed fert assumptions from H to L
               mort = -0.5, 
               int.mig = c(seq(1.125, 1.5, 0.125), rep(1.5, 14)), 
               recl = c(seq(1.125, 1.5, 0.125), rep(1.5, 14)), 
               edu = 1)
  SSP5 <- list(fert = c(seq(0.95, 0.8, -0.05), seq(0.7875, 0.75, -0.0125), rep(0.75, 10)), 
               mort = 0.5, 
               int.mig = c(seq(1.125, 1.5, 0.125), rep(1.5, 14)), 
               recl = c(seq(0.875, 0.5, -0.125), rep(0.5, 14)), 
               edu = 1)
  SSPs <- list(SSP1 = SSP1, SSP2 = SSP2, SSP3 = SSP3, SSP4 = SSP4, SSP5 = SSP5)
    
  SSP <- SSPs[[SSP.name]]
  
  if(any(sapply(list(fert, mort, int.mig, recl, edu), is.not.null))) SSP <- "SSP6"
 
  path.1 <- "output_data"
  path.2 <- paste(patt, "_", SSP.name, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), sep = "")
  if (!dir.exists(path.1)) dir.create(path.1, recursive = TRUE) #MW 2017-04-27 - works?
  dir.create(file.path(path.1, path.2))
  out.name <- file.path(path.1, path.2, path.2)
  
  if (is.not.null(fert)) {
    SSP$fert <- fert
  }
  if (SSP.name != "SSP2" | is.not.null(fert)) {
    if(length(SSP$fert) < 18) SSP$fert <- c(SSP$fert, rep(SSP$fert[length(SSP$fert)], 18 - length(SSP$fert)))
    asfredu.dt$value <- asfredu.dt$value * rep(SSP$fert, each = nrow(asfredu.dt) / iPr)
  }
  
  if (is.not.null(mort)) {
    SSP$mort <- mort
  }
  
  #unconditional computation of nsxedu:
  if (nSx == "le0") {
    if(length(SSP$mort) < 18) SSP$mort <- c(SSP$mort, rep(SSP$mort[length(SSP$mort)], 18 - length(SSP$mort)))
    SSP$mort <- cumsum(SSP$mort)
    nsxedu.dt <- le0.change(SSP$mort, out.name)
    sx.vars <- setdiff(intersect(names(popedu.dt), names(nsxedu.dt)), "value")
    nsxedu.dt <- nsxedu.dt[region != "india", .(var = "nSx", value = mean(value)), by = sx.vars]
    nsxedu.dt[, age := age - 5]
    if ("region" %in% p.vars) {
      nsxedu.dt$region <- factor(nsxedu.dt$region, labels = data[[pos.var.def]][variables == "region"]$values)
      }
    nsxedu.dt[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
    nsxedu.dt <- nsxedu.dt[, .(period, pattern, value)]
    setnames(nsxedu.dt, "value", "nSx")
  }
      
  if (is.not.null(int.mig)) { 
    SSP$int.mig <- int.mig
  }
  if (SSP.name != "SSP2" | is.not.null(int.mig)) {  
    if(length(SSP$int.mig) < 18) SSP$int.mig <- c(SSP$int.mig, rep(SSP$int.mig[length(SSP$int.mig)], 18 - length(SSP$int.mig)))
    #inter doesn't contain a time variable and thus is multiplied with the int.mig-values within the loop below
    }

  if (exists("reclass.dt")) {      
    if (is.not.null(recl)) { 
      SSP$recl <- recl
    }
    if (SSP.name != "SSP2" | is.not.null(recl)) {  
      if(length(SSP$recl) < 18) SSP$recl <- c(SSP$recl, rep(SSP$recl[length(SSP$recl)], 18 - length(SSP$recl)))
      #reclass.dt doesn't contain a time variable and thus is multiplied with the recl-values within the loop below
      }
    }  
  
  if (is.not.null(edu)) SSP$edu <- edu

  base.year <- min(popedu.dt$period)
  iPr.fin <- iPr
     
  for(iPr in 1:iPr.fin) {  
    print(paste("Period is ", iPr, sep = ""))
    
    #the following has to be done in every period because popedu.dt is derived from resProj within the simulation:
    popedu1 <- popedu.dt[period == base.year + 5 * (iPr - 1)][, var := NULL]
    setnames(popedu1, "value", "pop")
    nsxedu1 <- nsxedu.dt[period == base.year + 5 * (iPr - 1)][, period := NULL]
    projedu <- popedu1[nsxedu1, on = "pattern", nomatch = 0]
    projedu[, `:=` (pop1 = pop * nSx, deaths = pop * (1 - nSx))]

    if (is.not.null(ass.edu.dt)) {
      projedu[, areasex := do.call(paste, c(.SD, sep = "_")), .SDcols = intersect(names(projedu), areasex.vars)]
      #Check if all areas have a positive number of inhabitants. If not, eapr() is only executed for the ones with positive numbers:
      area.pos <- projedu[, .(popsum = sum(pop)), by = intersect(names(projedu), areasex.vars)]
      patt.pos <- unique(area.pos[, do.call(paste, c(.SD, sep = "_")), .SDcols = intersect(names(area.pos), c("region", "residence"))])
      if (length(patt.pos) > 0) {
        idx.pos <- sort((c(sapply(patt.pos, function(x) grep(x, projedu$areasex)))))
      } else {
        idx.pos <- 1:nrow(projedu)
      }
      projedu.pos <- data.table(ddply(projedu[idx.pos], .(areasex), eapr, ass.edu.dt = ass.edu.dt, maxeapr5 = maxeapr5))
      if (length(idx.pos) < nrow(projedu)) {
        projedu.0 <- projedu[setdiff(1:nrow(projedu), idx.pos)]
        projedu.0[, pop2 := pop1]
        projedu <- merge(projedu.pos, projedu.0, all = TRUE)
        #row reordering to match the intial order: 
        projedu <- projedu[match(popedu1$pattern, projedu$pattern), ]
        projedu <- projedu[, areasex := NULL]
      } else {
        projedu <- projedu.pos
      }
    } else {
      projedu[, pop2 := pop1]
    }
    
    #internal migration:
    if (exists("inter.dt")) {
      inter <- copy(inter.dt) 
      inter <- inter[, value := value * SSP$int.mig[iPr]]
      origin.vars <- intersect(names(projedu), c("region", "residence"))
      if (length(origin.vars) == 2) {
        projedu[, `:=` (origin = paste(region, residence, sep = "_"))]
        } else {
        projedu[, `:=` (origin = paste(projedu[[origin.vars]], sep = "_"))]
      }
      inter <- projedu[inter, on = intersect(names(inter), names(projedu)), nomatch = 0, allow.cartesian = TRUE]
      inter[, `:=` (mig = value * pop2 / 1000)]
      setkeyv(inter, intersect(c("sex", "age", "edu"), names(inter)))
      inter.out <- inter[, .(outmig = sum(mig)), by = c(key(inter), "origin")]
      inter.in <- inter[, .(inmig = sum(mig)), by = c(key(inter), "destination")] 
      resProj <- merge(projedu, inter.out, by = c(key(inter), "origin"), all.x = TRUE)
      resProj <- merge(resProj, inter.in, by.x = c(key(inter), "origin"), by.y = c(key(inter), "destination"), all.x = TRUE)
      for (j in c("outmig", "inmig")) set(resProj, which(is.na(resProj[[j]])), j, 0)
      resProj[, `:=` (pop3 = pop2 - outmig + inmig, origin = NULL)]
    } else {
      resProj <- copy(projedu)
      resProj[, pop3 := pop2]
      }

    asfr1 <- asfredu.dt[period == base.year + 5 * (iPr - 1)][, period := NULL]
    resProj <- merge(resProj, asfr1, by = "pattern", all.x = TRUE)
    
    p2.vars <- c(names(resProj)[1:(which(names(resProj) == "pop") - 1)], "pop3")
    popedu2 <- resProj[, p2.vars, with = FALSE]
    popedu2[, age := age + 5]
    popedu2[age == 105, age := 100]
    popedu2[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
    popedu2 <- rbind(popedu2[age < 100], popedu2[age >= 100, .(pop3 = sum(pop3)), by = setdiff(names(popedu2), c("pop3"))])
    setnames(popedu2, "pop3", "pop3.shift")
    popedu2[, pattern := do.call(paste, c(.SD, sep = "_")), .SDcols = p.vars]
    
    resProj <- merge(resProj, popedu2[, .(pattern, pop3.shift)], by = "pattern", all.x = TRUE)

    for (j in c("asfr", "pop3.shift")) set(resProj, which(is.na(resProj[[j]])), j, 0)    
    resProj[, births := asfr * (pop + pop3.shift) / 2 / 200]
    area.vars <- c("region", "residence")
    if (any(area.vars %in% names(resProj))) {
      resProj[, area := do.call(paste, c(.SD, sep = "_")), .SDcols = intersect(names(projedu), area.vars)]
    } else {
      resProj[, area := country]
    }
    
    pop.04 <- resProj[, .(births = sum(births)), by = area]
    sexRatio <- sexratio.dt[, .(area = area, sexr = sexr + ((1 / 1.05) * 1000 - sexr) / 8 * ifelse(iPr > 8, 8, iPr))]
    pop.04 <- pop.04[sexRatio, on = "area"]

    s0.male <- nsxedu1[grep("\\bmale_-5", nsxedu1$pattern)]
    s0.female <- nsxedu1[grep("female_-5", nsxedu1$pattern)]
    #only one nSx is used for all edu levels:
    if (any(pop.04$area != country)) { #= if region and/or residence are given
      s0.male <- s0.male[sapply(pop.04$area, function(x) grep(x, s0.male$pattern)[1]), ]
      s0.female <- s0.female[sapply(pop.04$area, function(x) grep(x, s0.female$pattern)[1]), ]
      pop.04[, c("pattern.male", "s0.male", "pattern.female", "s0.female") := list(s0.male[sapply(pop.04$area, function(x) grep(x, s0.male$pattern)), pattern],
                                                                                   s0.male[sapply(pop.04$area, function(x) grep(x, s0.male$pattern)), nSx],
                                                                                   s0.female[sapply(pop.04$area, function(x) grep(x, s0.female$pattern)), pattern],
                                                                                   s0.female[sapply(pop.04$area, function(x) grep(x, s0.female$pattern)), nSx])]
      pop.04[, c("surv.male", "dead.male", "surv.female", "dead.female") := list(births * 1000 / (1000 + sexr) * s0.male,
                                                                                 births * 1000 / (1000 + sexr) * (1 - s0.male),
                                                                                 births * sexr / (1000 + sexr) * s0.female,
                                                                                 births * sexr / (1000 + sexr) * (1 - s0.female))]
    } else {
      s0.male <- s0.male[1, ]
      s0.female <- s0.female[1, ]
      pop.04[, c("pattern.male", "s0.male", "pattern.female", "s0.female") := list(s0.male$pattern, s0.male$nSx, s0.female$pattern, s0.female$nSx)]
      pop.04[, c("surv.male", "dead.male", "surv.female", "dead.female") := list(births * 1000 / (1000 + sexr) * s0.male,
                                                                                 births * 1000 / (1000 + sexr) * (1 - s0.male),
                                                                                 births * sexr / (1000 + sexr) * s0.female,
                                                                                 births * sexr / (1000 + sexr) * (1 - s0.female))] 
      }

    pop.04.d <- melt(pop.04, measure = patterns("^surv", "^dead", "^pattern"), value.name = c("pop3.shift", "deaths.nb", "pattern"))
    pop.04.d$pattern <- sapply(pop.04.d$pattern, function(x) sub("-5", "0", x))
    
    resProj <- pop.04.d[, .(deaths.nb, pattern)][resProj, on = "pattern", nomatch = NA]
    resProj[match(pop.04.d$pattern, resProj$pattern), pop3.shift := pop.04.d$pop3.shift]
    setcolorder(resProj, c(names(resProj)[-1], "deaths.nb"))
    resProj[is.na(deaths.nb) == TRUE, deaths.nb := 0L]
       
    if (exists("reclass.dt")) {
      proprur <- resProj[, .("pop2" = sum(pop2)), area]
      if (nrow(proprur) > 2) { #regions and residence given
        proprur[, c(area.vars) := tstrsplit(proprur$area, "_")]
        proprur <- dcast(proprur, region ~ residence, value.var = "pop2")
        proprur[, newperural := rural / (rural + urban)]
        proprur$gap <- reclass.dt$gap[match(proprur$region, reclass.dt$area)]
      } else {
        #instead of area = "rural", region = "rural" is used to allow the correct matching:
        proprur <- data.table(region = "rural", rural = proprur[area == "rural", pop2], urban = proprur[area == "urban", pop2], 
                              newperural = proprur[area == "rural", pop2] / sum(proprur$pop2), gap = reclass.dt$gap)
      }
      
      #conditional computation of reclasstr:
      if (exists("glmMigrReclass")) {
        proprur[, `:=` (reclasstr = (predict(glmMigrReclass, data.frame(perural = proprur$newperural), type = "response") + gap) * SSP$recl[iPr])] #1.05ms
        proprur[areas.oldtr$area, reclasstr := areas.oldtr$reclasstr]
        proprur <- proprur[, .(region, reclasstr = reclasstr / (reclass.period / 5))]
      } else {
        proprur <- proprur[, .(region, reclasstr = reclass.dt$reclasstr)]
      }
 
      resProj[, pop3.shift.total := rep(resProj[, .(sum(pop3.shift)), by = setdiff(p.vars, "residence")]$V1, each = 2)]
      resProj[, `:=` (perural = pop3.shift / pop3.shift.total), ]
      if ("region" %in% names(resProj)) {
        resProj <- resProj[proprur, on = "region", nomatch = NA]
      } else {
        resProj[, reclasstr := proprur$reclasstr]
      }

      setkey(resProj, "pattern") 
      resProj[residence == "rural", pop4 := (pop3.shift.total * perural) - (pop3.shift.total * perural * reclasstr)]
      resProj[residence == "urban", pop4 := (pop3.shift.total * perural) + (pop3.shift.total * (1 - perural) * reclasstr)]
      for (j in c("perural", "pop4")) set(resProj, which(is.na(resProj[[j]])), j, 0)
    } else {
      resProj[, pop4 := pop3.shift] 
      }
    
    setkeyv(resProj, p.vars)
    if(iPr == 1) results <- vector("list", iPr.fin + 1)
    results[[iPr]] <- resProj

    popedu.dt <- resProj[, c(intersect(names(popedu.dt), names(resProj))), with = FALSE]
    popedu.dt[, `:=` (period = period + 5, var = "pop", value = resProj$pop4)]
    
    if(iPr == iPr.fin) {
      results[[iPr + 1]] <- popedu.dt[, `:=` (pop = value, var = NULL, value = NULL)] #final period
    } else {
      popedu.dt.100p <- popedu.dt[age >= 100, .(value = sum(value)), by = setdiff(p.vars, "age")]
      popedu.dt[age == 100, value := popedu.dt.100p[, value]]
      popedu.dt <- popedu.dt[age <= 100]
    }
  }

  #round results:
  round.names <- intersect(c("pop", "pop1", "deaths", "pop2", "outmig", "inmig", "births", "deaths.nb", "pop3.shift"), names(resProj))
  lapply(results[1:iPr], function(x) x[, (round.names) := round(.SD), .SDcols = round.names])
  results[[iPr + 1]][, pop := round(pop)]
  
  #write .csv of full results with 3 added columns:
  res <- rbindlist(results, fill = TRUE)
  write.csv(res, paste(file.path(path.1, path.2, paste("popprojFull", path.2, sep = "_")), ".csv", sep = ""))
  
  #standard output:
  res.var <- intersect(c("pop", "births", "deaths", "outmig", "inmig", "pop3.shift"), names(resProj))
  res.list1 <- vector("list", length(res.var))
  res.list1 <- lapply(seq_along(res.var), function(x) {
    res.list1[[x]] <- xtabs(as.formula(paste(res.var[x], "~", paste(c("period", "sex", intersect(names(resProj), area.vars)), collapse = "+"))), data = res)
    })
  names(res.list1) <- res.var 

  if (exists("inter.dt")) {
    net.mig <- with(res.list1, (inmig) - (outmig))
    res.list1$net.mig <- net.mig
    }
  
  res.var <- c("pop3.shift", "pop4")
  res.list2 <- vector("list", length(res.var))
  res.list2 <- lapply(seq_along(res.var), function(x) {
    res.list2[[x]] <- xtabs(as.formula(paste(res.var[x], "~", paste(c("period", intersect(names(resProj), area.vars)), collapse = "+"))), data = res)
  })
  names(res.list2) <- res.var
  if ("residence" %in% names(resProj)) {
    if ("region" %in% names(resProj)) {
      res.list2$prop.pop <- round(margin.table(res.list2$pop3.shift, 1:3)[, , 2] / margin.table(res.list2$pop3.shift, 1:2), 4) #proportion of urban population
      res.list2$prop.reclass <- round((margin.table(res.list2$pop3.shift, 1:3)[, , 1] - margin.table(res.list2$pop4, 1:3)[, , 1]) / 
        margin.table(res.list2$pop3.shift, 1:3)[, , 1], 4)  #proportion of reclassified (rural)
    } else {
      res.list2$prop.pop <- round(margin.table(res.list2$pop3.shift, 1:2)[, 2] / margin.table(res.list2$pop3.shift, 1), 4)
      res.list2$prop.reclass <- round((margin.table(res.list2$pop3.shift, 1:2)[, 1] - margin.table(res.list2$pop4, 1:2)[, 1]) / margin.table(res.list2$pop3.shift, 1:2)[, 1], 4)
    }
    res.list <- c(res.list1[c(1:3, 7)], res.list2[-(1:2)], SSP)
  } else {
    res.list <- res.list1[c(1:3, 7)]
  }
  
  saveRDS(res.list, paste(out.name, ".rds", sep = "")) 
  
  if(detail.out == TRUE & iPr.fin > 1) msproj.out(res, res.list, res.list1, out.name, p.vars, country, area.vars)
  
  return(res.list)
}
