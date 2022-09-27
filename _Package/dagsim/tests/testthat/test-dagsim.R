test_that("dagsim generates data", {
  Foo <- dagsim(list(x ~ eps(.3), y ~ x + eps(.3)))
  expect_equal(nrow(Foo), 10)
  expect_equal(names(Foo), c("x", "y"))
})

test_that("nrows is used", {
  Foo <- dagsim(list(x ~ eps(.5), y ~ x + eps(.5)), nrow=10000)
  expect_equal(nrow(Foo), 10000)
  expect_equal(sum(is.na(Foo$y)), 0)
})

test_that("eps() generates noise of the right magnitude", {
  Foo <- dagsim(list(x ~ eps(.5), y ~ x + eps(.5)), nrow=10000)
  expect_lt(abs(sd(Foo$x) - 0.5), 0.1)
  expect_lt(abs(sd(Foo$y) - sqrt(0.5)), 0.1)
})

test_that("unif() generates the uniform-distribution.", {
  Foo <- dagsim(list(x ~ unif(min=-2, max=2), prob ~ punif(x, min=-2, max=2)), nrow=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("tdist() generates the t-distribution.", {
  Foo <- dagsim(list(x ~ tdist(df=1), prob ~ pt(x, df=1)), nrow=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("seq() generates a sequence", {
  Foo <- dagsim(list(x ~ seq()), nrow=100)
  expect_equal(Foo$x, 1:100)
})

test_that("Constant patterns are replicated to have nrows", {
  Foo <- dagsim(list(x ~ 3), nrow=10)
  expect_equal(nrow(Foo), 10)
  Foo2 <- dagsim(list(x ~ 1:3), nrow=10)
  expect_equal(sum(Foo2$x == 1), 4)
})

test_that("Names starting with dots don't appear in the output.", {
  Foo <- dagsim(list(.genes ~ eps(), x ~ .genes + eps(), y ~ .genes + eps()))
  expect_equal(names(Foo), c("x", "y") )
})
