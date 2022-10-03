test_that("set module works", {
  expect_equal(
    set_difference(strsplit("hello", NULL), letters), strsplit("hello", NULL)
  )
})
