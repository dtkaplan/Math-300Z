#' A library of DAGS. These are available in data/.
#'
dag01 <- list(
  x ~ eps(),
  y ~ x + eps()
)

dag02 <- list(
  x ~ eps(),
  a ~ eps(),
  y ~ x + a + eps()
)

dag03 <- list(
  .c ~ eps(),
  x ~ .c + eps(),
  y ~ .c + eps()
)

save(dag01, dag02, dag03,
     file = "data/daglib.rda")
