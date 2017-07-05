vars <- c("variables", "values", "reclasstr", "perural", "area", "sexr", "variable", "pattern", "axmxvars", "period", 
          "value", "areasex", "age", "region", "pop", "pop2", "pop1", "residence", "mig", "outmig", "inmig", "pop3", 
          "pop3.shift", "births", "asfr", "deaths.nb", "newperural", "rural", "urban", "glmMigrReclass", "gap", 
          "pop3.shift.total", "pop4", "sex", "TFR", "edu", "perc", "all.edu", "origin", "destination", "popsum")
funcs <- c("le0.change", "patterns", "msproj.out")

if(getRversion() >= "2.15.1") utils::globalVariables(c(vars, funcs))