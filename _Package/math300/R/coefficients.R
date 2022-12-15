#' Return the coefficients from a model in the form of a data frame
#'
#' This is a wrapper around stats::coefficients()
#' configured to put the name of each coefficient
#' in the "terms" column.
#'
#' @export
coefficients <- function(mod) {
  Tmp <- stats::coefficients(mod)

  data.frame(term = names(Tmp),
             coefficient=as.numeric(Tmp))
}
