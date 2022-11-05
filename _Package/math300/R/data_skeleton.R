#' Convert a data frame to a skeleton
#'
#' Only explanatory variables are included in the result. Use `expand.grid()` on
#' the output to create a data frame with the skeleton values.
#'
#' @param data a data frame
#' @param tilde the model specification as a tilde expression
#' @param spreadn integer, number of the explanatory variables to give multiple levels
#' @param \ldots specific levels to use for variables
#' @param ncont number of levels at which to represent a continuous variable that comes first in the formula
#' @param nlevels number of levels at which to represent other variables
#'

#' @export
data_skeleton <- function(data, tilde, spreadn=NULL, ..., ncont=10, nlevels=3) {
  # find the names of the explanatory variables in an order implied by the formula
  explan_names <- all.vars(tilde)
  # strip out the response variable, if any
  if (length(tilde) == 3) explan_names <- explan_names[-1]

  # How many variables are to have multiple values in the skeleton
  if (is.null(spreadn)) spreadn <- min(c(length(explan_names), 4))
  by_hand <- list(...)

  if (!all(explan_names %in% names(data))) stop("<tilde> has names not in the data.")

  # Placeholders
  Vals <- list()

  # is the first explanatory variable continuous or discrete?
  first_vals <- data[[explan_names[1]]]
  first_type <- continuous_or_discrete(first_vals)
  Vals[[explan_names[[1]]]] <- get_typical(data[[explan_names[1]]], type="continuous", ncont=ncont, nlevels=nlevels)
  if (spreadn > 1) {
    for (k in 2:spreadn){
      Vals[[explan_names[[k]]]] <-
        get_typical(data[[explan_names[k]]], type="discrete",
                    ncont=ncont, nlevels=nlevels)

    }
  }

  if (length(explan_names) > spreadn) {
    for (k in (spreadn+1):length(explan_names)) {
      Vals[[explan_names[[k]]]] <- get_typical(data[[explan_names[k]]], type="discrete", nlevels=1, ncont=1)
    }
  }

  if (length(by_hand) > 0) {
    for (k in 1:length(by_hand)) {
      Vals[[names(by_hand)[k]]] <- by_hand[[k]]
    }
  }

  Vals # all combinations of the levels

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

    n <- ifelse(type=="continuous", ncont, nlevels)
    breaks <- compromise_breaks(vals, n = n)
    return(breaks)
  } else {
    # pull out the nlevels most populated levels
    biggest <- names(sort(table(vals)))[1:nlevels]
    biggest[!is.na(biggest)]
  }
}

#' @export
compromise_breaks <- function(vals, n) {
  breaks = quantile(vals, seq(0, 1, length=n), na.rm=TRUE)
  # evenly spread throughout the value range
  breaks2 <- seq(min(vals, na.rm=TRUE), max(vals, na.rm=TRUE), length=n)

  # compromise
  as.numeric((breaks + breaks2)/2)
}
