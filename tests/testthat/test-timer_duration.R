set.seed(52)
x <- sort(60 ^ runif(100, 0, 3.3))

test_that("timer_duration", {
  expect_snapshot(
    print(as.timer_duration(floor(x)))
  )
})

test_that("timer_duration with decimals", {
  expect_snapshot(
    print(as.timer_duration(round(x, 0:2)))
  )
})

test_that("negative timer_duration with decimals", {
  expect_snapshot(
    print(as.timer_duration(floor(x)) * c(1, -1))
  )
})

test_that("negative timer_duration with decimals", {
  expect_snapshot(
    print(as.timer_duration(round(x, 0:2)) * c(1, -1))
  )
})
