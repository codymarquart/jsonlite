context("toJSON AsIs")

test_that("Encoding AsIs", {
  expect_that(toJSON1(list(1), auto_unbox=TRUE), equals("[1]"));
  expect_that(toJSON1(list(I(1)), auto_unbox=TRUE), equals("[[1]]"));
  expect_that(toJSON1(I(list(1)), auto_unbox=TRUE), equals("[1]"));

  expect_that(toJSON1(list(x=1)), equals("{\"x\":[1]}"));
  expect_that(toJSON1(list(x=1), auto_unbox=TRUE), equals("{\"x\":1}"));
  expect_that(toJSON1(list(x=I(1)), auto_unbox=TRUE), equals("{\"x\":[1]}"));

  expect_that(toJSON1(list(x=I(list(1))), auto_unbox=TRUE), equals("{\"x\":[1]}"));
  expect_that(toJSON1(list(x=list(I(1))), auto_unbox=TRUE), equals("{\"x\":[[1]]}"));
});
