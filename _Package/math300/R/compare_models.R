#' Compare models
#'
#' Compare a set of models, all with the same response variable, for RMS prediction error.
#'
#' @param DAG A DAG, as made by dag_make()
#' @param \ldots One or more model formulas, all with the same response variable.
#' @param n Sample size from DAG.
#' @param in_sample Boolean flag: whether to use in-sample testing. Default: FALSE.
#'
#'
#' @export


compare_rms_error <- function(DAG, ...,  n=500, in_sample=FALSE) {
  # collect the models
  models <- list(...)
  # make sure they all have the same response variable
  responses <- unique(unlist(lapply(models, function(x) all.names(x[[2]]))))
  if (length(responses) > 1) stop("All model formulas must have the same response variable.")
  else response <- as.name(responses) # convert to a name so it can be inserted in the formula
  Training <- sample(DAG, size=n)
  if (in_sample) Testing <- Training
  else Testing <- sample(DAG, size=2*n) # a bit bigger, because we can
  res <- numeric(length(models))
  for (k in 1:length(models)) {
    mod <- lm(models[[k]], data = Training)
    rms <- mod_eval(mod, data = Testing) %>%
      summarize(rms = sqrt(mean((!!response - model_output)^2)))
    res[k] <- rms$rms # pull out of the data frame
  }
  return(res)
}
