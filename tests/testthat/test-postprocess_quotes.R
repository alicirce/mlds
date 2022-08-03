test_that("html formatted url works", {
  expect_equal(
    html_link("https://www.marxists.org"),
    '<a href="https://www.marxists.org">link</a>'
  )
  # with custom link text
  expect_equal(
    html_link("https://www.marxists.org", "source"),
    '<a href="https://www.marxists.org">source</a>'
  )
})

test_that("url generation works - marx capital", {
  expect_equal(
    url_marx_cap(6),
    "https://www.marxists.org/archive/marx/works/1867-c1/ch06.htm"
  )
  expect_equal(
    url_marx_cap(13),
    "https://www.marxists.org/archive/marx/works/1867-c1/ch13.htm"
  )
})

test_that("url generation works - leninature", {
  expect_equal(
    url_leninature("works/1897/econroman/i8ps.htm"),
    "https://www.marxists.org/archive/lenin/works/1897/econroman/i8ps.htm"
  )
})
