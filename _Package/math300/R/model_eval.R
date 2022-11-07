#' Evaluate a model on an input data set.
#'
#'
#' @export
model_eval <- function(mod, data=NULL, type=c("response", "link"),
                       interval=c("prediction", "confidence", "none"), level=0.95,
                       skeleton=FALSE) {
  type <- match.arg(type)
  interval = match.arg(interval)
  if (level <= 0 || level >=1) stop("<level> must be > 0 and < 1.")

  response_var_name <- as.character(deparse(mosaicModel:::response_var(mod)))

  if (skeleton) {
    eval_data <- model_skeleton(mod, data=data)
    response_in_data <- FALSE
  } else {
    if (is.null(data)) {
      training_data <- extract_training_data(mod)
      eval_data <- model.frame(mod)
      response_in_data <- TRUE
    } else {
      eval_data <- training_data <- data
      # the argument data might or might not have the response name
      response_in_data <- response_var_name %in% names(data)
    }
  }

  # Evaluate the model at the selected data values
  interval_fun <- add_pi
  if (interval == "confidence") interval_fun = add_ci
  # Try to get a prediction interval
  Result <- try(
    interval_fun(eval_data, mod, yhatName=".output",
                 names=c(".lwr", ".upr"), alpha=1-level, response=TRUE),
    silent=TRUE
    )
  if (inherits(Result, "try-error")) {
    if (interval=="prediction")
      warning("Prediction intervals not available for this model type. Giving confidence intervals instead.")
    Result <- add_ci(eval_data, mod, alpha = 1 - level,
                     names=c(".lwr", ".upr"), yhatName=".output",
                     response=TRUE)
    }
  Fitted <- Result[".output"]
  if (".lwr" %in% names(Result)) Result <- Result[c(".lwr", ".upr")]

  if (response_in_data) {
    Residuals <- data.frame(.resid = eval_data[[1]] - Fitted$.output)
    return(bind_cols(training_data, Fitted, Residuals,  Result))
  } else {
    return(bind_cols(training_data, Fitted, Result))
  }
}


