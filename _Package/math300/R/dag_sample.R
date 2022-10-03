#' Generate data from a DAG
#'
#' A DAG is represented as a list of formulas. The right-hand side
#' gives the name of the variable. Variables starting with dots will not be
#' printed.
#'
#' @param DAG list of formulas. Variables must be listed before they
#' can be used to create other variables.
#' @param size size of the sample, that is, the number of rows to be put in the data frame
#' @param seed Set the random number seed. Useful for reproducibility.
#'
#' @examples
#' dag_sample(dag03)
#'
#' @importFrom tibble as_tibble
#' @export
dag_sample <- function(DAG, size=10, seed=NULL) {
  # check that DAG is a list of formulas
  if (!is.list(DAG)) stop("DAG must be a list of formulas")
  if (!all(unlist(lapply(DAG, function(x) inherits(x, "formula")))))
    stop("All the components of DAG must be formulas.")

  # random noise generators
  eps <- function(sd = 1) {
    rnorm(size, mean=0, sd=sd)
  }
  tdist <- function(df=3, ncp=0) {
    rt(size, df=df, ncp=ncp)
  }
  unif <- function(min=0, max=1) {
    runif(size, min=min, max=max)
  }
  roll <- function(levels=1:6, weight=rep(1, length(levels))) {
    replicate(size, sample(levels, size=1, prob=weight))
  }
  each <- function(expr) {
    expr <- substitute(expr)
    replicate(size, eval(expr))
  }

  #transformations
  binom <- function(x=0) {
    # 1 or 0 output with logistic input
    prob <- exp(x)/(1+exp(x))
    as.numeric(runif(size) < prob)
  }
  seq <- function() 1:size

  # set random number generator seed, if called for
  if (!is.null(seed)) set.seed(seed)

  vnames <- lapply(DAG, function(x) all.names(x[[2]])) |> unlist()

  Res <- list()

  # make sure they are sorted to depend only on the rows above.

  for (k in 1:length(DAG)) {
    # Carry out the operation assumed by the formula
    rhs <- rlang::f_rhs(DAG[[k]])

    this <- eval(rhs, envir = Res)
    if (length(this) != size) this <- rep_len(this, size)

    Res[[vnames[k]]] <- this # make it available for successive formulas.
  }

  # Post-process: take out the items whose names start with dots
  keepers <- !grepl("^\\.", vnames)

  Res <- Res[keepers]


  tibble::as_tibble(Res)

}

#' @importFrom mosaic sample
#' @export
sample.dagsystem <- function(x, size, replace = FALSE, ...) {
  if (missing(size)) size=5
  dag_sample(x, size=size, ...)
}

