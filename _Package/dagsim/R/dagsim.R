#' Generate data from a DAG
#'
#' A DAG is represented as a list of formulas. The right-hand side
#' gives the name of the variable. Variables starting with dots will not be
#' printed.
#'
#' @param DAG list of formulas. Variables must be listed before they
#' can be used to create other variables.
#' @param nrow Number of rows in the data frame
#' @param seed Set the random number seed. Useful for reproducibility.
#'
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
#'
#' @examples
#' dagsim(list(x ~ eps(0.5), y ~ x + eps(0.5)))
#' dagsim(list(.genes ~ eps(), x ~ .genes + eps(), y ~ .genes + eps()))
#' dagsim(list(x ~ c("a", "b", "c"), y ~ 2), nrow=5) # e.g. for blocking
#' dagsim(list(x ~ roll(c("a", "b", "c"))), nrow=5)
#'
#' @importFrom tibble as_tibble
#' @export
dagsim <- function(DAG, nrow=10, seed=NULL) {
  # check that DAG is a list of formulas
  if (!is.list(DAG)) stop("DAG must be a list of formulas")
  if (!all(unlist(lapply(DAG, function(x) inherits(x, "formula")))))
    stop("All the components of DAG must be formulas.")

  # random noise generators
  eps <- function(sd = 1) {
    rnorm(nrow, mean=0, sd=sd)
  }
  tdist <- function(df=3, ncp=0) {
    rt(nrow, df=df, ncp=ncp)
  }
  unif <- function(min=0, max=1) {
    runif(nrow, min=min, max=max)
  }
  roll <- function(levels=1:6, weight=rep(1, length(levels))) {
    replicate(nrow, sample(levels, size=1, prob=weight))
  }
  each <- function(expr) {
    expr <- substitute(expr)
    replicate(nrow, eval(expr))
  }

  #transformations
  binom <- function(x) {
    # 1 or 0 output with logistic input
    prob <- exp(x)/(1+exp(x))
    as.numeric(runif(nrow) < prob)
  }
  seq <- function() 1:nrow

  # set random number generator seed, if called for
  if (!is.null(seed)) set.seed(seed)

  vnames <- lapply(DAG, function(x) all.names(x[[2]])) |> unlist()

  Res <- list()

  # make sure they are sorted to depend only on the rows above.

  for (k in 1:length(DAG)) {
    # Carry out the operation assumed by the formula
    rhs <- rlang::f_rhs(DAG[[k]])

    this <- eval(rhs, envir = Res)
    if (length(this) != nrow) this <- rep_len(this, nrow)

    Res[[vnames[k]]] <- this # make it available for successive formulas.
  }

  # Post-process: take out the items whose names start with dots
  keepers <- !grepl("^\\.", vnames)

  Res <- Res[keepers]


  tibble::as_tibble(Res)

}


