#' Construct a DAG object
#'
#' DAG objects are made from formulas. Each formula has a variable name
#' on the left-hand size, and an expression on the right-hand side. The expression is to
#' to be written using variables defined in earlier formulas, as well as random
#' number generators, etc.

#' @details Some random number generators are built in, and can be used in the DAG.
#' - `eps(sd=1)` --- normally distributed with the given standard deviation
#' - `unif(min=0, max=1)` --- uniformly distributed
#' - `tdist(df=3, ncp=0)` --- t-distributed (long tails)
#' - `roll(levels=1:6, weights=1)` --- generate samples from a vector
#' - `each(tilde)` --- run the same instructions anew for each row
#' Also,
#' - `seq()` --- sequence from 1 to nrow
#' - `binom(input)` --- generates a 0/1 variable based on the magnitude of the input. Probability of 1 is a logistic transformation
#' of the input.
#' Use `sample()` to collect data from a DAG. Arguments: `size=`, `seed=``
#'
#' @param \dots one or more formulas in the DAG format
#' puts the right class on a dag, so that sample will work.
#' @examples
#' one <- makeDag(x ~ eps(0.5), y ~ x + eps(0.5))
#' two <- makeDag(.genes ~ eps(), x ~ .genes + eps(), y ~ .genes + eps()))
#' three <- makeDag(x ~ c("a", "b", "c"), y ~ 2)) # e.g. for blocking
#' four <- makeDag(x ~ roll(c("a", "b", "c"))))
#' sample(one, size=5)
#' @export
makeDag <- function(...) {
  # TO DO. Check the formulas.


  res <- list(...)
  class(res) <- "dagsystem"

  res
}
