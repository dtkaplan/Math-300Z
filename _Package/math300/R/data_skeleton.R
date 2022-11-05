#' Convert a data frame to a skeleton
#'
#' Only explanatory variables are included in the result.
#'
#' @param data a data frame
#' @param tilde the model specification as a tilde expression
#' @param \ldots specific levels to use for variables
#' @param ncont number of levels at which to represent a continuous variable that comes first in the formula
#' @param nlevels number of levels at which to represent other variables
#'
#' @export
data_skeleton <- function(data, tilde, ..., ncont=100, nlevels=3) {
  # find the names of the explanatory variables in an order implied by the formula
  explan_names <- all.vars(tilde)
  by_hand <- list(...)

  if (!all(explan_names %in% names(data))) stop("<tilde> has names not in the data.")

  # strip out the response variable, if any
  if (length(tilde) == 3) explan_names <- explan_names[-1]

  # Placeholders
  Vals <- list()

  # is the first explanatory variable continuous or discrete?
  first_vals <- data[[explan_names[1]]]
  first_type <- continuous_or_discrete(first_vals)
  Vals[[explan_names[[1]]]] <- get_typical(data[[explan_names[1]]], type="continuous", ncont=ncont, nlevels=nlevels)
  if (length(explan_names) > 1)
    Vals[[explan_names[[2]]]] <- get_typical(data[[explan_names[2]]], type="discrete", ncont=ncont, nlevels=nlevels)
  if (length(explan_names) > 2)
    Vals[[explan_names[[3]]]] <- get_typical(data[[explan_names[3]]], type="discrete", ncont=ncont, nlevels=nlevels)
  if (length(explan_names) > 3)
    Vals[[explan_names[[4]]]] <- get_typical(data[[explan_names[4]]], type="discrete", ncont=ncont, nlevels=nlevels)

  if (length(explan_names) > 4) {
    for (k in 4:length(explan_names)) {
      Vals[[explan_names[[k]]]] <- get_typical(data[[explan_names[k]]], type="discrete", nlevels=1, ncont=1)
    }
  }

  if (length(by_hand) > 0) {
    for (k in 1:length(by_hand)) {
      Vals[[names(by_hand)[k]]] <- by_hand[[k]]
    }
  }

  expand.grid(Vals) # all combinations of the levels

}

#' @export
continuous_or_discrete <- function(vals) {
  ifelse(inherits(vals, c("character", "logical", "factor", "zero_one")),
         "discrete",
         "continuous")
}

#' @export
get_typical <- function(vals, type=c("continuous", "discrete"), ncont=10, nlevels=3) {
  type <- match.arg(type)
  # special cases
  if (inherits(vals, "zero_one")) return(unique.zero_one(vals))
  if (inherits(vals, "logical")) return(unique(vals))

  val_type <- continuous_or_discrete(vals)
  if (val_type=="continuous") {
    if (ncont==1) return(median(vals, na.rm=TRUE))
    bounds <- quantile(vals, c(0.10, 0.90), na.rm=TRUE)
    n <- ifelse(type=="continuous", ncont, nlevels)
    return(seq(bounds[1], bounds[2], length=n))
  } else {
    # pull out the nlevels most populated levels
    biggest <- names(sort(table(vals)))[1:nlevels]
    biggest[!is.na(biggest)]
  }
}

