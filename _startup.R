# load packages needed for the reading notes and NTIs

library(knitr)
library(kableExtra)
library(ggformula) # Change plots to ggplot2
library(tibble)
library(dplyr)
library(mosaic)
library(mosaicModel)
library(dagsim)
knitr::opts_chunk$set(echo = TRUE)



##################
# compare a set of models, all with the same response variable, for RMS prediction error.
# This needs to go in a Math300 package!
compare_rms_error <- function(DAG, ...,  n=500, in_sample=FALSE) {
  # collect the models
  models <- list(...)
  # make sure they all have the same response variable
  responses <- unique(unlist(lapply(models, function(x) all.names(x[[2]]))))
  if (length(responses) > 1) stop("All model formulas must have the same response variable.")
  else response <- as.name(responses) # convert to a name so it can be inserted in the formula
  Training <- sample(DAG, size=n)
  if (in_sample) Testing <- Training
  else Testing <- sample(DAG, size=2*n) # a bit bigger, because we can
  res <- numeric(length(models))
  for (k in 1:length(models)) {
    mod <- lm(models[[k]], data = Training)
    rms <- mod_eval(mod, data = Testing) %>%
      summarize(rms = sqrt(mean((!!response - model_output)^2)))
    res[k] <- rms$rms # pull out of the data frame
  }
  return(res)
}
