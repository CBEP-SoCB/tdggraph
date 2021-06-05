library(tdggraph)
data("dep_sonde")

test_that("Function produces ggplot2 plot", {
expect_s3_class(ptlines(dep_sonde, temp, depth, month), 'ggplot')
})

test_that("Bad data frame throws error", {
  expect_error(ptlines(.dt = "bicycle", .val = temp, .y = depth, .grp = month),
               "is.data.frame")
})

test_that("Bad reference to data throws error", {
  expect_error(ptlines(dep_sonde, temp, depth, hiccup))
})

# There is no easy way to check consistency of output.
# testthat::expect_snapshot() and testthat::expect_snapshot_value() can't
# successfully serialize a ggplot object. package `vdiffr` may do the
# job, at the cost of yet another dependency.

# Instead, we could look at only certain components of the plot
# we can pull out specific parts, like labels, mapping, layers,
# or use the `proto` package to access details.
# see https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot


# It also appears that testthat does not pick up errors propagated from
# `rlang::eval_tidy()`
# test_that("bad reference to data throws error", {
# expect_error(ptlines(dep_sonde, banana, depth, month), 'eval_tidy')
# })
