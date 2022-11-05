#' Plot model output for representative levels of explanatory variables
#'
#' @export
model_plot <- function(mod, data = NULL, tilde=NULL, nlevels=3) {
  if (is.null(data)) data <- mosaicModel::data_from_mod(mod)
  if (is.null(tilde)) tilde <- mosaicModel::formula_from_mod(mod)
  plotting_names <- all.vars(tilde)
  if (length(tilde) == 3) plotting_names <- plotting_names[-1]
  other_explan_names <- all.vars(mosaicModel::formula_from_mod(mod)[[3]])
  all_names <- union(plotting_names, other_explan_names)
  all_names_formula <- as.formula(paste("~", paste(all_names, collapse="+")))
  # data_skeleton() never returns the response variable
  skeleton <- model_eval(mod,
                         data=data_skeleton(data, all_names_formula, nlevels=nlevels))

  space_formula <- as.formula(glue::glue(".output~ {plotting_names[1]}"))

  if(length(plotting_names) > 1) {
    color_formula <- as.formula(glue::glue("~ {plotting_names[2]}"))
  } else color_formula <- "blue"

  P <- gf_point(space_formula, color=color_formula, data = skeleton, size=0.75)
  if (length(plotting_names) > 2) {
    # prepare to set up the facets
    if (length(plotting_names) == 3) P <- P + facet_wrap(plotting_names[3], labeller = "label_both")
    else P <- P + facet_grid(rows=names(skeleton)[3], cols=plotting_names[4], labeller = "label_both")
  }

  P

}
