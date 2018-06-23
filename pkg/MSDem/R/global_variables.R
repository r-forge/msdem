vars <- c("variables", "values", "reclasstr", "perural", "area", "sexr", "variable", "pattern", "axmxvars", "period", 
          "value", "areasex", "age", "region", "pop", "pop2", "pop1", "residence", "mig", "outmig", "inmig", "pop3", 
          "emi", "immi", "pop4", "births", "asfr", "pop4.shift", "deaths.nb", "newperural", "rural", "urban", 
          "glmMigrReclass", "gap", "pop4.shift.total", "pop5", "sex", "TFR", "edu", "perc", "all.edu", "origin", 
          "destination", "popsum", "ini", "adj.fac", "mort.diff_u15", "mort.diff", "mort.ediff")
funcs <- c("le0.change", "patterns", "msproj.out")

if(getRversion() >= "2.15.1") utils::globalVariables(c(vars, funcs))