#' Generate a regression report in the frm of a data frame.
#'
#' @details This is simply a wrapper around `broom::tidy()` used to
#' emphasize to students that the results are a summary in the form of a regression
#' report, similar to the summaries produced by `confint()`, `coefficients()`, etc.
#'
#' @param model A model as produced by `lm()`, `glm()`, and so on
#'
#' @export
regression_summary <- function(model, ...) {
  broom::tidy(model)
}
