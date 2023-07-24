set.seed(52)
x <- as.breaktime_duration(sort(60 ^ runif(100, 0, 3.3)))

test_that("breaktime_duration", {
  expect_snapshot(
    print(x)
  )
})

test_that("breaktime_duration with decimals", {
  expect_snapshot(
    print(x, digits = 2)
  )
})

test_that("negative breaktime_duration", {
  expect_snapshot(
    print(x * c(1, -1))
  )
})

test_that("negative breaktime_duration with decimals", {
  expect_snapshot(
    print(x * c(1, -1), digits = 2)
  )
})
