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

## Graphics routines for classifiers and hypothesis testing
get_condition <- function(p=0.2, n=50, letters = c("C", "H")) {
  tibble::tibble(
    cond = c(rep(letters[1], p*n), rep(letters[2], (1-p)*n)),
    index = c(1:(p*n), 1:((1-p)*n))
  )
}

getxy <- function(n=50, thresh=1.5) {
  tibble::tibble(
    x = rnorm(n),
    y = rnorm(n),
    L = sqrt(x^2 + y^2)
  ) %>% mutate(
    offset = runif(n, .9, 1)^4,
    x = ifelse(L > thresh, (thresh*offset)*x/L, x),
    y = ifelse(L > thresh, (thresh*offset)*y/L, y)
  ) %>%
    select(x,y)
}
get_testresult <- function(condition,
                           letters=c("C","H"),
                           probs=c(.9, .15)) {
  n <- length(condition)
  rand <- runif(n)
  test_result <- ifelse(condition==letters[1], rand < probs[1],
                        rand < probs[2])
  raw_result <- ifelse(condition==letters[1], sqrt(rexp(n,0.1)), sqrt(rexp(n,0.6)))
  tibble::tibble(
    raw_result = raw_result^2,
    test_result = c("-", "+")[test_result+1]
  ) %>%
    mutate(raw_result = pmin(40, raw_result))
}
get_shape <- function(test_result, group, condition, letters=c("C", "H")) {
  # filled for + test result
  group + 15*(condition==letters[1])
}

patient_group <- function(group = 0, n=50, letters=c("C", "H"),
                          p = 0.2, probs=c(.9, .15)){
  if (!group %in% 0:2) stop("Group must be 0, 1, 2")
  Cond <- get_condition(p, n, letters)
  Test <- get_testresult(Cond$cond, letters, probs)
  Res <- bind_cols(Cond, Test, getxy(n)) %>%
    mutate(
      shape = get_shape(test, group, cond, letters),
      raw_shape = ifelse(shape >=15, shape-15, shape),
      color = ifelse(test_result=="+", "blue", "gray")
    )

  Res
}
# patient groups

Pg1 <- patient_group(0, n=1000, p=0.05)
People <- bind_rows(
  Pg1,
  patient_group(1, n=600, p=0.1),
  patient_group(2, n=400, p=0.2)
)

Case_control <- People %>% filter(index<=50)

plot_people_final <- function(data) {
  gf_point(y ~ x, shape = ~shape, color=~color, data=data) +
    scale_shape_identity(guide="none") +
    scale_color_identity(guide="none") +
    theme_void() + coord_fixed()
}

plot_people_score <- function(data) {
  gf_point(y ~ x, shape=~raw_shape, color=~raw_result, data=data) +
    scale_shape_identity(guide="none") +
    scale_color_gradient(low="#A0A0A0", high="#0000FF", guide="none") +
    theme_void() + coord_fixed()
}

plot_people_score_condition <- function(data) {
  gf_point(y ~ x, shape = ~shape, color=~raw_result, data=data) +
    scale_shape_identity(guide="none") +
    scale_color_gradient(low="#A0A0A0", high="#0000FF", guide="none") +
    theme_void() + coord_fixed()
}




