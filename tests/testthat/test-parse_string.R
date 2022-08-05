test_that("string input parsing works", {
  expected_list <- list(c("linen", "coat"))
  expect_equal(parse_string("linen, coat"), expected_list)
  expect_equal(parse_string("linen,coat"), expected_list)
  expect_equal(parse_string("linen,coat\n"), expected_list)
})

test_that("string input parsing works: multi-line", {
  # with and without comma at end of new line
  multi_line1 <- "surplus-value,surplus value,\nlabour-power,labour power"
  multi_line2 <- "surplus-value,surplus value\nlabour-power,labour power"
  expected_list <- list(
    c("surplus-value", "surplus value"),
    c("labour-power", "labour power")
  )
  expect_equal(parse_string(multi_line1), expected_list)
  expect_equal(parse_string(multi_line2), expected_list)
})

test_that("string input null handling", {
  expect_null(parse_string(""))
  expect_null(parse_string(character(0)))
  expect_null(parse_string(NULL))
})

test_that("group like words returns expected data frame", {
  words_in <- list(
    c("surplus-value", "surplus value"),
    c("labour-power", "labour power")
  )
  words_out <- group_same_words(words_in)
  expect_true(inherits(words_out, "data.frame"))
  expect_named(words_out, c("word", "keyword"))
  expect_true(all(unlist(words_in) %in% words_out$word))
  expect_equal(
    as.character(sort(unique(words_out$keyword))),
    c("surplus-value", "labour-power")
  )
})

test_that("group like words handles null", {
  words_in <- NULL
  words_out <- group_same_words(words_in)
  expect_true(inherits(words_out, "data.frame"))
  expect_named(words_out, c("word", "keyword"))
  expect_true(nrow(words_out) == 0)
})
