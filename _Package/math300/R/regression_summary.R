#' Generate a regression or ANOVA report in the form of a data frame.
#'
#' @details These are simply wrappers around `broom::tidy()` used to
#' emphasize to students that the results are a summary in the form of a regression
#' report, similar to the summaries produced by `confint()`, `coefficients()`, etc.
#'
#' @param model A model as produced by `lm()`, `glm()`, and so on
#'
#' @export
regression_summary <- function(model, ...) {
  broom::tidy(model)
}

#' @rdname regression_summary
#' @export
anova_summary <- function(...) {
  broom::tidy(anova(...))
}

