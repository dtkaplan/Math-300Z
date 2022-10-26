# load packages needed for the reading notes and NTIs

library(knitr)
library(kableExtra)
library(ggformula) # Change plots to ggplot2
library(tibble)
library(dplyr)
library(mosaic)
library(mosaicModel)
library(math300)
library(moderndive)

knitr::opts_chunk$set(echo = TRUE)

# Functions for transcluding content
LC_file <- function(n) {
  glue::glue("../LC/LC-lesson{n}.qmd")
}
Obj_file <- function(n) {
  glue::glue("../Objectives/Obj-lesson-{n}.qmd")
}


## Typesetting functions

# a temporary replacement for the index_entry() used in the SM2 book
index_entry <- function(...) return(NULL)
