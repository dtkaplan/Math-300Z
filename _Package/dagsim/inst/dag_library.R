#' A library of DAGS. These are available in data/.
#'
dag01 <- dag_make(
  x ~ eps(),
  y ~ 1.5*x + 4.0 + eps()
)


dag02 <- dag_make(
  x ~ eps(),
  a ~ eps(),
  y ~ 3*x - 1.5*a + 5 +  eps()
)

dag03 <- dag_make(
  g ~ eps(),
  x ~ 1.0*g + eps(),
  y ~ 1.0*g + eps()
)

save(dag01, dag02, dag03,
     file = "data/daglib.rda")

# An experiment

exp_test <- dag_make(
  g ~ eps(),
  x ~ g + eps(),
  y ~ binom()
)
