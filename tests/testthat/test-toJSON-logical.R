context("toJSON Logical")

test_that("Encoding Logical", {
  expect_that(toJSON1(TRUE), equals("[true]"));
  expect_that(toJSON1(FALSE), equals("[false]"));
  expect_that(toJSON1(as.logical(NA)), equals("[null]"))
  expect_that(toJSON1(as.logical(NA), na="string"), equals("[\"NA\"]"))
  expect_that(toJSON1(c(TRUE, NA, FALSE)), equals("[true,null,false]"));
  expect_that(toJSON1(c(TRUE, NA, FALSE), na="string"), equals("[true,\"NA\",false]"));
  expect_that(toJSON1(logical()), equals("[]"));
});

test_that("Encoding Logical in Data Frame", {
  expect_that(toJSON1(data.frame(foo=TRUE)), equals("[{\"foo\":true}]"));
  expect_that(toJSON1(data.frame(foo=FALSE)), equals("[{\"foo\":false}]"));
  expect_that(toJSON1(data.frame(foo=as.logical(NA))), equals("[{}]"));
  expect_that(toJSON1(data.frame(foo=as.logical(NA)), na="null"), equals("[{\"foo\":null}]"));
  expect_that(toJSON1(data.frame(foo=as.logical(NA)), na="string"), equals("[{\"foo\":\"NA\"}]"));
  expect_that(toJSON1(data.frame(foo=c(TRUE, NA, FALSE))), equals("[{\"foo\":true},{},{\"foo\":false}]"));
  expect_that(toJSON1(data.frame(foo=logical())), equals("[]"));
});
