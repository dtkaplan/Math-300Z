#' train a model, easily
#'
#' An interface to several of the most often used model-fitting routines
#' designed to make it easy to construct
#'
#' @param data Data frame to use as training data
#' @param tilde Formula for the model
#' @param type Character string: the type of model to fit
#' @param verbose whether to report on decisions being made
#' @param prob_of which of two levels to use for logistic regression
#' @export
model_train <- function(data, tilde, verbose=FALSE, prob_of = NULL,
                      type = c("auto", "linear", "prob", "counts"), logs=FALSE, ...) {
  type <- match.arg(type)
  response_vals <- eval(tilde[[2]], envir=data)
  # if we already know what type of model ...
  if (type=="linear") {
    mod <- lm(tilde, data = data)
    return(mod)
  } else if (type == "prob") {
    # is the response categorical

    if (!is.logical(response_vals)) {
      if (!is.numeric(response_vals)) {
        if (!is.null(prob_of) && prob_of %in% unique(response_vals))
          response_vals <- response_vals == prob_of
        else {
          # use the first of the levels
          yes_level <- unique(response_vals)[1]
          message(paste("Probability of", yes_level, "is output from model."))
          response_vals <- response_vals == yes_level
        }
      } else {
        if (max(response_vals) > 1 || min(response_vals) < 0)
          stop("Response variable has values outside the range [0,1]. Not suitable
               for logistic (probability) regression.")
      }
    }
    data$.response <- response_vals
    tilde[[2]] <- as.name(".response")
    mod <- glm(tilde, data=data, family="binomial")
    return(mod)
  } else if (type=="counts") {
    if (min(response_vals) < 0) stop("Can't do binomial regression on negative response values.")
    mod <- glm(tilde, data = data, family="poisson")
    return(mod)
  } else if (type=="auto") {
    # Figure out what the response variable is
    if (is.numeric(response_vals)) {
      if (min(response_vals) >= 0 &&
          all(response_vals==round(response_vals) )) type = "counts"
      else type = "linear"
    } else if (is.logical(response_vals) || !is.numeric(response_vals)) {
      type = "prob"
    }
    return(Recall(data, tilde, verbose=verbose, prob_of=prob_of, type=type, logs=logs))
  } else {
    stop("Model type not yet implemented.")
  }




}
