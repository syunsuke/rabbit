test_that("japanese 2 unicode", {
  f <- sub_japanese2unicode_filter

  input <- '愛'
  expect <- '\\u611b'
  expect_equal(f(input),expect)

  input <- '猫 #愛ねこ　犬'
  expect <- '\\u732b #愛ねこ　犬'
  expect_equal(f(input),expect)

  input <- '\\u732b\n猫'
  expect <- '\\u732b\n\\u732b'
  expect_equal(f(input),expect)

  input <-  'replacement = "\\\\\\\\u$1") %>%'
  expect <- 'replacement = "\\\\\\\\u$1") %>%'
  expect_equal(f(input),expect)
})


