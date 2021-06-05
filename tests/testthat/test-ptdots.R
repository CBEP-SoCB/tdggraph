library(tdggraph)
data("dep_sonde")

test_that("Function produces ggplot2 plot", {
  expect_s3_class(ptdots(dep_sonde, sample_date, depth, temp), 'ggplot')
})

test_that("Bad data frame throws error", {
  expect_error(ptdots(.dt = "bicycle", sample_date, depth, temp),
               "is.data.frame")
})

#test_that("bad reference to data throws error", {
#  expect_error(ptdots(dep_sonde, temp, depth, hiccup))
#})
