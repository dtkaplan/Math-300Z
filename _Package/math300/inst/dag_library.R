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

dag04 <- dag_make(
  a ~ eps(),
  b ~ eps(),
  c ~ eps(),
  d ~ a + b + c + eps()
)

dag05 <- dag_make(
  a ~ eps(),
  b ~ a + eps(),
  c ~ b + eps(),
  d ~ c + eps()
)

dag06 <- dag_make(
  a ~ eps(),
  b ~ a + eps(),
  c ~ b + eps(),
  d ~ c + a + eps()
)

dag07 <- dag_make(
  a ~ eps(),
  b ~ eps() - a,
  c ~ a - b + eps(),
  d ~ eps()
)

save(dag01, dag02, dag03, dag04, dag05, dag06, dag07,
     file = "data/daglib.rda")

# An experiment

exp_test <- dag_make(
  g ~ eps(),
  x ~ g + eps(),
  y ~ binom()
)
