#' A library of DAGS. These are available in data/.
#'
#'
#'
library(math300)


dag00 <- dag_make(
  x ~ eps(2) + 5,
  y ~ eps(1) - 7
)

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

dag08 <- dag_make(
  c ~ eps(),
  x ~ c + eps(),
  y ~ x + c + 3 + eps()
)

dag09 <- dag_make(
  a ~ eps(),
  b ~ eps(),
  c ~ binom(2*a+ 3*b)
)


# a case-control style data source. There are roughly
# even numbers of 0s and 1s. Only a, b, c have an impact on y
dag10 <- dag_make(
  a ~ eps(),
  b ~ eps(),
  c ~ eps(),
  d ~ eps(),
  e ~ eps(),
  f ~ 2*binom() - 1,
  y ~ binom(2*a - 3*b + c + 0*d + 0*e + 0*f)
)

dag11 <- dag_make(
  x ~ eps(),
  y ~ eps(),
  g ~ x + y + eps()
)

dag12 <- dag_make(
  x ~ eps(),
  y ~ eps(),
  h ~ x + y,
  g ~- h + eps()
)

dag_school1 <- dag_make(
  expenditure ~ unif(7000, 18000),
  participation ~ unif(1,100),
  outcome ~ 1100 + 0.01*expenditure - 4*participation + eps(50)
)

dag_school2 <- dag_make(
  culture ~ unif(-1, 1),
  expenditure ~ 12000 + 4000 * culture + eps(1000),
  participation ~ (50 + 30 * culture + eps(15)) %>%
    pmax(0) %>% pmin(100),
  outcome ~ 1100 + 0.01*expenditure - 4*participation + eps(50)
)

dag_vaccine <- dag_make(
  .h ~ eps(1),
  .v ~ 0.2 + 2* .h + eps(.25),
  .f ~ -0.5 - 0.5 * binom(.v) - 1*.h,
  .s ~ 2 - 0.2*binom(.f) + 0.4*(.h + 0.5),
  died ~ binom(.s, labels=c("yes", "no")),
  vaccinated ~ binom(.v, labels=c("none", "yes")),
  health ~ binom(.h, labels=c("poor", "good")),
  flu ~ binom(.f)
)

save(dag00, dag01, dag02, dag03, dag04, dag05,
     dag06, dag07, dag08, dag09, dag10, dag11, dag12,
     dag_vaccine,
     dag_school1, dag_school2,
     file = "data/daglib.rda")

# An experiment

exp_test <- dag_make(
  g ~ eps(),
  x ~ g + eps(),
  y ~ binom()
)
