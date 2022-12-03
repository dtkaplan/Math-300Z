#' R-squared and allied numbers
#'
#' A convenience function to calculate Rsquared and F from a model, along with
#' the degrees of freedom in the numerator and denominator
#'
#' @param mod A model generated with `lm()`
#' @export
R2 <- function(mod) {
  r2 <- mosaic::rsquared(mod)
  k <- length(na.omit(mod$coefficients)) - 1
  n <- k + mod$df.residual
  f <- ((r2)/k) / ((1-r2)/(n-k))

  data.frame(n=n, k=k, Rsquared=r2, F=f)
}

