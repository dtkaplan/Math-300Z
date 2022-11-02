#' Compare models based on the size of their residuals
#'
#' Compare a set of models, all with the same response variable, using either
#' RMS residuals or the sum-of-square residuals
#'
#' @param source A data frame or a DAG (as made by dag_make())
#' @param \ldots One or more model formulas, all with the same response variable.
#' @param measure Either of `"RMS"` (the default) or `"SS"` (sum of squares)
#' @param n Sample size from DAG.
#' @param in_sample Boolean flag: whether to use in-sample testing. Default: FALSE.
#'
#'
#' @examples
#' compare_model_residuals(mtcars, mpg ~ 1, mpg ~ hp, mpg ~ hp + wt, measure="SS", in_sample=TRUE)
#' compare_model_residuals(dag07, c ~ 1, c ~ a, c ~ a + b, c ~ a + b + d, measure="SS", in_sample=FALSE)

#' @export

compare_model_residuals <- function(source, ...,  n=500,
                                    measure = c("RMS", "SS"), in_sample=FALSE) {
  # collect the models
  models <- list(...)
  measure <- match.arg(measure) # make sure it's one of the allowed possibilities
  # make sure they all have the same response variable
  responses <- unique(unlist(lapply(models, function(x) all.names(x[[2]]))))
  if (length(responses) > 1) stop("All model formulas must have the same response variable.")
  else response <- as.name(responses) # convert to a name so it can be inserted in the formula
  if (inherits(source, "dagsystem")) {
    Training <- sample(source, size=n)
    if (in_sample) Testing <- Training
    else Testing <- sample(source, size=2*n) # a bit bigger, because we can
  } else {
    if (!in_sample) stop("Only in-sample testing available on a data frame.")
    Training <- Testing <- source
  }
  res <- numeric(length(models))
  for (k in 1:length(models)) {
    mod <- lm(models[[k]], data = Training)
    Mod_evaluated <- mosaicModel::mod_eval(mod, data = Testing)
    if (measure == "RMS") {
      rms <- Mod_evaluated %>%
        summarize(rms = sqrt(mean((!!response - model_output)^2)))
      res[k] <- rms$rms # pull out of the data frame
    } else if (measure == "SS") {
      ss <- Mod_evaluated %>%
        summarize(ss = sum((!!response - model_output)^2))
      res[k] <- ss$ss

    } else stop("Should never get here! Report this message to package maintainer.")
  }
  return(res)
}
