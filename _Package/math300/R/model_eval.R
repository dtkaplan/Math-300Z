#' Evaluate a model on an input data set.
#'
#'
#' @export
model_eval <- function(mod, data=NULL, type=c("response", "link"),
                       interval=c("prediction", "confidence", "none"), level=0.95) {
  type <- match.arg(type)
  interval = match.arg(interval)
  if (level <= 0 || level >=1) stop("<level> must be > 0 and < 1.")

  if (is.null(data)) data <- model.frame(mod)


  ## NEED TO UPDATE THE RESPONSE_Var and explanatory_vars functions
  ## to return a character vector of variable names
  keepers <- c(all.names(mosaicModel:::response_var(mod)),
               unlist(mosaicModel:::explanatory_vars(mod)))
  # Keep only the input columns needed by the model
  data <- data[ , keepers]
  if (grepl("^zero_one\\(.*\\)$", names(data)[1])) {
    names(data)[1] <- gsub("^zero_one\\((.*)\\)$", "\\1", names(data)[1])
  }
  interval_fun <- add_pi
  if (interval == "confidence") interval_fun = add_ci
  # Try to get a prediction interval
  Result <- try(
    interval_fun(data, mod, yhatName=".output",
                 names=c(".lwr", ".upr"), alpha=1-level),
    silent=TRUE
    )
  if (inherits(Result, "try-error")) {
    if (interval=="prediction")
      warning("Prediction intervals not available for this model type.")
    Result <- add_ci(data, mod, yhatName=".output", names=c(".lwr", ".upr"),
                     alpha = 1 - level)
    # drop interval if none requested
    if (interval != "confidence") Result <- dplyr::select(Result, -.lwr, -.upr)
  }
  Fitted <- dplyr::select(Result, .output)
  Result[ , c(".output", names(data))] <- NULL
  Residuals <- data.frame(.resid = data[[1]] - Fitted$.output)

  bind_cols(data, Fitted, Residuals,  Result)
}


