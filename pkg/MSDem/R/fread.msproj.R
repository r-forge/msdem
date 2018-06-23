fread.msproj <- function(data.dir = "input_data/", model.patt = NULL) {
  data.names <- list.files(data.dir, pattern = model.patt)
  data.names <- data.names[str_replace_all(data.names, c("_mig_dom|_mig_int|_var_def|_state_space|_axmx|.csv"), "") == model.patt]
  datasets <- lapply(paste(data.dir, data.names, sep = "/"), function(x) 
    fread(x, showProgress = FALSE, stringsAsFactors = TRUE, na.strings = c("NA", "#N/A")))
  names(datasets) <- sapply(data.names, function(x) gsub(".csv", "", x))
  return(datasets)
}