context("toJSON NULL values")

test_that("Test NULL values", {
  namedlist <- structure(list(), .Names = character(0));
  x <- NULL
  y <- list(a=NULL, b=NA)
  z <- list(a=1, b=character(0))

  expect_that(validate(toJSON1(x)), is_true())
  expect_that(fromJSON(toJSON1(x)), equals(namedlist))
  expect_that(toJSON1(x), equals("{}"))
  expect_that(toJSON1(x, null="list"), equals("{}"))

  expect_that(validate(toJSON1(y)), is_true())
  expect_that(toJSON1(y, null="list"), equals("{\"a\":{},\"b\":[null]}"))
  expect_that(toJSON1(y, null="null"), equals("{\"a\":null,\"b\":[null]}"))
  expect_that(fromJSON(toJSON1(y, null="null")), equals(y))
  expect_that(fromJSON(toJSON1(y, null="list")), equals(list(a=namedlist, b=NA)))

  expect_that(validate(toJSON1(z)), is_true())
  expect_that(toJSON1(z), equals("{\"a\":[1],\"b\":[]}"))
  expect_that(fromJSON(toJSON1(z)), equals(list(a=1, b=list())))
})
