#-----------
#Alina Skripets
#Testing
#-----------
library(testthat)
#Include at least four expectations for each function (e.g. expect_equal())
source('./functions.R')
#-----------
vec <- c(1,2,3,4,5,NA,NA)

context("testing remove missing")
test_that('testing remove missing', {
expect_equivalent(remove_missing(vec), na.omit(vec))
expect_equal(remove_missing(c(1, 2, 3)), c(1,2,3))
expect_equal(remove_missing(c(1,2,3,NA)), c(1,2,3))
expect_equal(remove_missing(c(NA, NA, NA)), numeric(0))
})

context("testing get_minimum")
test_that('testing get_minimum', {
expect_error(get_minimum(c('a', 1, 2, 3)))
expect_equal(get_minimum(c(1, 2, 3)), 1)
expect_equal(get_minimum(c(1,2,3,NA)), get_minimum(c(1,2,3)))
expect_equal(length(get_minimum(vec)), 1)
})


context("testing get_maximum")
test_that('testing get_maximum', {
expect_error(get_maximum(c('a', 1, 2, 3)))
expect_equal(get_maximum(c(1, 2, 3)), 3)
expect_equal(get_maximum(c(1,2,3,NA)), get_maximum(c(1,2,3)))
expect_equal(length(get_maximum(vec)), 1)
})

context("testing get_range")
test_that('testing get_range', {
expect_error(get_range(c('a', 1, 2, 3)))
expect_equal(get_range(c(1, 2, 3)), 2)
expect_equal(get_range(c(1,2,3,NA)), get_range(c(1,2,3)))
expect_equal(length(get_range(vec)), 1)
})

context("testing get_percentile10")
test_that('testing get_percentile10', {
expect_error(get_percentile10(c('a', 1, 2, 3)))
expect_equivalent(get_percentile10(c(1, 2, 3)), quantile(c(1, 2, 3), 0.1))
expect_equal(get_percentile10(c(1,2,3,NA)), get_percentile10(c(1,2,3)))
expect_equal(length(get_percentile10(vec)), 1)
})

context("testing get_percentile90")
test_that('testing get_percentile90', {
expect_error(get_percentile90(c('a', 1, 2, 3)))
expect_equivalent(get_percentile90(c(1, 2, 3)), quantile(c(1, 2, 3), 0.9))
expect_equal(get_percentile90(c(1,2,3,NA)), get_percentile90(c(1,2,3)))
expect_equal(length(get_percentile90(vec)), 1)
})

context("testing get_median")
test_that('testing get_median', {
expect_error(get_median(c('a', 1, 2, 3)))
expect_equivalent(get_median(c(1, 2, 3)), quantile(c(1, 2, 3), 0.5))
expect_equal(get_median(c(1,2,3,NA)), get_median(c(1,2,3)))
expect_equal(length(get_median(vec)), 1)
})


context("testing get_average")
test_that('testing get_average', {
expect_error(get_average(c('a', 1, 2, 3)))
expect_equivalent(get_average(c(1, 2, 3)), mean(c(1, 2, 3)))
expect_equal(get_average(c(1,2,3,NA)), get_average(c(1,2,3)))
expect_equal(length(get_average(vec)), 1)
})

context("testing get_stdev")
test_that('testing get_stdev', {
expect_error(get_stdev(c('a', 1, 2, 3)))
expect_equivalent(get_stdev(c(1, 2, 3)), sd(c(1, 2, 3)))
expect_equal(get_stdev(c(1,2,3,NA)), get_stdev(c(1,2,3)))
expect_equal(length(get_stdev(vec)), 1)
})

context("testing get_quartile1")
test_that('testing get_quartile1', {
expect_error(get_quartile1(c('a', 1, 2, 3)))
expect_equivalent(get_quartile1(c(1, 2, 3)), quantile(c(1, 2, 3), 0.25))
expect_equal(get_quartile1(c(1,2,3,NA)), get_quartile1(c(1,2,3)))
expect_equal(length(get_quartile1(vec)), 1)
})

context("testing get_quartile3")
test_that('testing get_quartile3', {
expect_error(get_quartile3(c('a', 1, 2, 3)))
expect_equivalent(get_quartile3(c(1, 2, 3)), quantile(c(1, 2, 3), 0.75))
expect_equal(get_quartile3(c(1,2,3,NA)), get_quartile3(c(1,2,3)))
expect_equal(length(get_quartile3(vec)), 1)
})

context("testing count_missing")
test_that('testing count_missing', {
expect_error(count_missing(c('a', 1, 2, 3)))
expect_equivalent(count_missing(vec), sum(is.na(vec)))
expect_equal(count_missing(c(1,2,3,NA, NA, NA)), 3)
expect_equal(length(count_missing(vec)), 1)
})

context("testing summary_stats")
test_that('testing summary_stats', {
expect_error(summary_stats(c('a', 1, 2, 3)))
expect_equivalent(typeof(summary_stats(vec)), typeof(list()))
expect_equal(length(summary_stats(vec)), 11)
expect_identical(names(summary_stats(vec)), 
c("minimum", "percent10", "quartile1", "median", "mean", "quartile3", 
  "percent90", "maximum", "range", "stdev", "missing"))
})


context("testing print_stats")
test_that('testing print_stats', {
expect_error(print_stats(c('a', 1, 2, 3)))
expect_equivalent(typeof(print_stats(vec)), "NULL")
expect_equal(length(print_stats(vec)), 0)
expect_error(print_stats(list(c(1,3), "b", 3)))
})

context("testing rescale100")
test_that('testing rescale100', {
expect_error(rescale100(c('a', 1, 2, 3), 0, 100))
expect_equivalent(rescale100(vec, 0, 100), vec)
expect_equal(typeof(rescale100(c(1,2,3), 0, 100)), 'double')
expect_equal(length(rescale100(vec, 0, 100)), length(vec))
})

context("testing drop_lowest")
test_that('testing drop_lowest', {
expect_identical(drop_lowest(c(1,1,1)), c(1,1))
expect_equivalent(drop_lowest(c(0, 100)), 100)
expect_equal(length(drop_lowest(vec)), length(vec)-1)
expect_equal(sum(vec)-sum(drop_lowest(vec)), min(vec))
})

context("testing score_homework")
test_that('testing score_homework', {
expect_false(score_homework(vec, 1) == score_homework(vec, 2))
expect_equivalent(score_homework(c(0, 100)), 100)
expect_equal(length(score_homework(vec)), 1)
expect_gt(score_homework(vec, 1), score_homework(vec,2))
})


context("testing score_quiz")
test_that('testing score_quiz', {
expect_false(score_quiz(vec, 1) == score_quiz(vec, 2))
expect_equivalent(score_quiz(c(0, 100)), 100)
expect_equal(length(score_quiz(vec)), 1)
expect_gt(score_quiz(vec, 1), score_quiz(vec,2))
})

context("testing score_lab")
test_that('testing score_lab', {
expect_type(score_lab(1), 'double')
expect_identical(score_lab(11), score_lab(12))
expect_equal(length(score_lab(7)), 1)
expect_gt(score_lab(10), score_lab(7))
})
