#Function generates three or four files: 
# - variable definitions ('country_scenario_var_def.csv')
# - empty state space ('country_scenario_state_space.csv')
# - One or two migration files (only international or domestic plus international) ('country_scenario_mig_dom.csv' and/or 'country_scenario_mig_int.csv')

state.space <- function(period = c(2010, 2100), by = 5, region = NULL, residence = c("rural", "urban"), sex = c("male", "female"),
                        age = c(0, 100), edu = NULL, dom = "biregional", dom.var = "mrate", int.emi.var = "mrate", country = "Country", 
                        scen = "SSP2", data.dir = "input_data/") {
  is.not.null <- function(x) !is.null(x)
  "%nin%" <- function(x, y) which(!x %in% y)
  
  #generate age sequence out of the given values and add 1 for the life table (if steps are not annual):
  age <- unique(sort(c(seq(age[1], age[2], by), 1)))
  
  #generate period sequence and delete last value of 'period':
  period <- seq(period[1], period[2], by)
  period <- period[- length(period)]
  
  #Create directory for data files:
    if (data.dir == "input_data") {
      input.dir <- file.path(getwd(), "input_data")
    } else {
      input.dir <- data.dir
      if (str_sub(input.dir, start = - 1) != "/") input.dir <- file.path(input.dir, "/")
      }
  if (!dir.exists(input.dir)) dir.create(input.dir, recursive = TRUE)
 
  #only use edu if the number of levels was entered by the user:
  if (is.not.null(edu)) edu = paste("e", 1:edu, sep = "")
  
  #variable definitions:
  vars <- list(period = period, region = region, residence = residence, sex = sex, age = age, edu = edu)
  use.vars <- vars[sapply(vars, function(x) is.not.null(x))]
  var.def <- cbind(variables = "country", values = country)
  var.def <- rbind(var.def, matrix(c(rep(names(use.vars), lengths(use.vars)), unlist(use.vars, use.names = FALSE)), ncol = 2))
  #since region and residence are placed in the columns, expand.grid doesn't use these spatial variables anymore:
  spatial.vars <- c("region", "residence")
  if (is.not.null(region) | is.not.null(residence)) {
    spatial.cols <- apply(expand.grid(use.vars[names(use.vars) %in% spatial.vars]), 1, function(x) paste(x, collapse = "_"))
  } else {
    spatial.cols <- "value"
  }
  st.sp.vars <- use.vars[names(use.vars) %nin% spatial.vars]
    
  #state space and standard variables:
  st.sp <- expand.grid(rev(st.sp.vars))[, length(st.sp.vars):1]
  pop <- cbind(subset(st.sp, period == min(period)), 
               var = factor("pop", levels = c("pop", "le0", "mx", "ax", "asfr", "sexr", "reclasstr", "gap", "perural", "eapr")), 
               matrix(NA, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
  if (by == 5) pop <- subset(pop, age != 1)
  le0 <- cbind(subset(st.sp, age == 0), var = "le0", matrix(NA, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
  mx <- cbind(st.sp, var = "mx", matrix(NA, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
  ax <- cbind(st.sp, var = "ax", matrix(NA, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols))) 
  mort <- rbind(le0, mx, ax)
  fert <- cbind(subset(st.sp, age %in% 15:45 & sex == "female"), var = "asfr", matrix(NA, ncol = length(spatial.cols), 
                                                                                      dimnames = list(NULL, spatial.cols)))
  #expand state.space by sex ratio:
  sexr <- data.frame(period = period, matrix(NA, nrow = length(period), ncol = ncol(st.sp) - 1, dimnames = list(NULL, colnames(st.sp)[-1])), 
                var = c("sexr"), 
                matrix(NA, nrow = length(period), ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
  
  if (is.not.null(region) | is.not.null(residence)) {
    use.vars.mig <- c(st.sp.vars, list(origin = spatial.cols))
    if (by == 5) use.vars.mig$age <- setdiff(use.vars.mig$age, 1)
    use.vars.mig$period <- min(use.vars.mig$period)

    #international migration (many-to-one/one-to-many):
    use.vars.mig$origin <- c(use.vars.mig$origin, "World")
    im <- expand.grid(rev(use.vars.mig))[, length(use.vars.mig):1]
    use.vars.mig$destination <- use.vars.mig$origin
    im <- expand.grid(rev(use.vars.mig))[, length(use.vars.mig):1]
    im <- subset(im, xor(origin == "World", destination == "World")) #no further distinction of cases needed
    im <- cbind(im, var = factor("mabs", levels = c("mabs", "mrate")), value = NA)
    im$var[im$origin != "World"] <- int.emi.var
    rownames(im) <- 1:nrow(im)    

    #domestic migration is possible if we have either region or residence or both
    #biregional (region from/to rest) / bilateral (region from/to every other region) choice
    #for bilateral domestic migration, a matrix (wide) format is used; for biregional, the standard (long) format is kept    
    if (dom == "bilateral") { #for bilateral, region must be there!
      use.vars.mig <- use.vars.mig[names(use.vars.mig) != "destination"]
      use.vars.mig$origin <- setdiff(use.vars.mig$origin, "World")
      dm <- expand.grid(rev(use.vars.mig))[, length(use.vars.mig):1]
      dm <- cbind(dm, var = dom.var, matrix(NA, ncol = length(use.vars.mig$origin), dimnames = list(NULL, use.vars.mig$origin)))
    } else { 
      idx <- names(im) %nin% c("var", "value")
      dm <- im[ ,idx]
      dm <- cbind(dm, setNames(data.frame(NA), dom.var))
      idx <- which(names(dm) %in% c("origin", "destination"))
      dm[, idx] <- lapply(dm[, idx], function(x) gsub("World", country, x))
    }
    
    #reclassification:
      if (is.not.null(residence)) {
        reclass <- data.frame(matrix(NA, nrow = 3, ncol = ncol(st.sp), dimnames = list(NULL, colnames(st.sp))), 
                              var = c("reclasstr", "gap", "perural"), 
                              matrix(NA, nrow = 3, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
    #update var.def:
        var.def <- rbind(var.def, c("reclass", "TRUE"))
      } else {
        var.def <- rbind(var.def, c("reclass", "FALSE"))    
      }
    
      write.csv(dm, paste(input.dir, country, "_", scen, "_mig_dom.csv", sep = ""), row.names = FALSE)
    } else { #international migration (one-to-one):
      use.vars.mig <- c(st.sp.vars, list(origin = c(country, "World")))
      if (by == 5) use.vars.mig$age <- setdiff(use.vars.mig$age, 1)
      use.vars.mig$period <- min(use.vars.mig$period)
      im <- expand.grid(rev(use.vars.mig))[, length(use.vars.mig):1]
      use.vars.mig$destination <- use.vars.mig$origin
      im <- expand.grid(rev(use.vars.mig))[, length(use.vars.mig):1]
      im <- subset(im, origin != destination)
      im <- cbind(im, var = factor("mabs", levels = c("mabs", "mrate")), value = NA)
      im$var[im$origin != "World"] <- int.emi.var
      rownames(im) <- 1:nrow(im)         
    }

  write.csv(im, paste(input.dir, country, "_", scen, "_mig_int.csv", sep = ""), row.names = FALSE)
    
  #education:
  if (is.not.null(edu)) {
    edu.len <- length(edu)
    edu.mat <- network.initialize(edu.len)
    row.idx <- rep(1:(edu.len - 1), each = 2)
    col.idx <- row.idx + rep(1:2, edu.len - 1)
    del.idx <- which(col.idx > edu.len)
    row.idx <- row.idx[- del.idx]
    col.idx <- col.idx[- del.idx]
    add.edges(edu.mat, row.idx, col.idx)
    edu.mat <- as.matrix(edu.mat)
    dimnames(edu.mat) <- list(vars$edu, vars$edu)
    #updated educ to match st.sp format:
    vars.edu <- c(st.sp.vars, list(edu.to = st.sp.vars$edu))
    #period has to be expanded because of the computations in eapr.R:
    vars.edu$period <- seq(min(period), by = 5, length.out = length(period) + 2)
    vars.edu$age <- seq(10, 30, 5)
    use.vars.edu <- vars.edu[sapply(vars.edu, function(x) is.not.null(x))]
    educ <- expand.grid(rev(use.vars.edu))[, length(use.vars.edu):1]
    
    edu.patt <- setdiff(paste("e", row.idx, "e", col.idx, sep = ""), "e1e3")
    educ <- educ[paste(educ$edu, educ$edu.to, sep = "") %in% edu.patt, ]  #only use possible transitions (overall)
    educ <- subset(educ, (age == 10 & edu == "e1") |
                         (age == 15 & edu %in% c("e1", "e2")) | 
                         (age == 20 & edu %in% c("e3", "e4", "e5")) | 
                         (age == 25 & edu %in% paste("e", 4:7, sep = "")) | 
                         (age == 30 & edu %in% paste("e", 5:7, sep = ""))) #only use possible transitions (specific for ages)
    educ$edu <- factor(paste(educ$edu, gsub("[[:alpha:]]", "", educ$edu.to), sep = ""))
    educ <- cbind(educ[, -ncol(educ)], var = "eapr", matrix(NA, ncol = length(spatial.cols), dimnames = list(NULL, spatial.cols)))
    
    #update var.def:
    var.def <- rbind(var.def, matrix(c(rep("edu", length(levels(educ$edu))), levels(educ$edu)), nrow = length(levels(educ$edu))))
    
    } else {
      edu.mat <- NULL
      }
  
  #Update var.def to include domestic migration:
  if (is.not.null(region) | is.not.null(residence)) {  
    var.def <- rbind(var.def, c("mig.dom", dom), c("dom.in.var", dom.var), c("dom.out.var", dom.var))
  } else {
    var.def <- rbind(var.def, c("mig.dom", "no domestic migration"))
    }  
  
  #Update var.def to include international migration (only biregional (one-to-one or one-to-many) at the moment):
  var.def <- rbind(var.def, c("mig.int", "biregional"), c("int.immi.var", "mabs"), c("int.emi.var", int.emi.var))
    
  #merging the files:
  st.sp <- rbind(pop, mort, fert, sexr)
  if (exists("reclass")) st.sp <- rbind(st.sp, reclass)
  if (exists("educ")) {
    levels(st.sp$edu) <- c(levels(st.sp$edu), levels(educ$edu))
    st.sp <- rbind(st.sp, educ)
  } 

  row.names(st.sp) <- 1:nrow(st.sp)
  write.csv(st.sp, paste(input.dir, country, "_", scen, "_state_space.csv", sep = ""), row.names = FALSE)    
  
  #add dimension of the files to var.def:
  var.def <- rbind(var.def, matrix(c("state_space.rows", "state_space.cols", dim(st.sp)), ncol = 2))
  if (is.not.null(region) | is.not.null(residence)) {  
    var.def <- rbind(var.def, matrix(c("mig_dom.rows", "mig_dom.cols", dim(dm)), ncol = 2))
  }   
  var.def <- rbind(var.def, matrix(c("mig_int.rows", "mig_int.cols", dim(im)), ncol = 2))
  
  write.csv(var.def, paste(input.dir, country, "_", scen, "_var_def.csv", sep = ""), row.names = FALSE) #doesn't include possible education transitions
  
  
  res <- list(state.space = st.sp, variable.definitions = var.def, mig.dom = NULL, mig.int = im, edu.trans = edu.mat)
  if (exists("dm")) {
    res$mig.dom <- dm
  } else {
    res <- res[names(res) != "mig.dom"]
  }
  return(res)
}
