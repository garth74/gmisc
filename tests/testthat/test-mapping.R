test_that("Mapping works", {
  objs <- list(
    setNames(letters, letters),
    as.list(setNames(letters, letters)),
    as.data.frame(as.list(setNames(letters, letters)))
  )
  for (i in seq_along(objs)) {
    m <- Mapping$new(objs[[i]])
    expect_equal(m['a'], "a")
    m['a'] <- 'hello'
    expect_equal(m['a'], 'hello')
    m['a'] <- 'a'
    expect_equal(m$size, 26)
    expect_equal(m$size, length(m))
    expect_equal(m$keys(), unname(objs[[2]]))
    expect_equal(as.list(m), objs[[2]])
    expect_equal(as.data.frame(m), objs[[3]])
  }
})
