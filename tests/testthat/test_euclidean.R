test_that("GDC is calculated correctly.", {
  expect_equal(euclid(123612, 13892347912), 4)
  expect_equal(euclid(100, 1000), 100)
  expect_equal(euclid(-100, 1000), 100)
})


test_that("Wrong input throws an error.", {
  expect_error(euclid("100", 1000))
  expect_error(euclid(100, "1000"))
  expect_error(euclid(TRUE, "1000"))
})