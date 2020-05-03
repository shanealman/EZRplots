test_that("easy_plot_ly works", {

  correct_result <- 'A'

  testPlot <- easy_plot_ly(x = mtcars$mpg, y = mtcars$hp, z = mtcars$drat, type = 'scatter',
                           color = mtcars$cyl, data = mtcars)

  my_result <- testPlot$x$source

  expect_equal(my_result, correct_result)

})


test_that("add_reg_plane works", {

  correct_result <- "A"

  testPlot <- easy_plot_ly(x = mtcars$mpg, y = mtcars$hp, z = mtcars$drat, type = 'scatter',
                           color = mtcars$cyl, data = mtcars)

  testReg <- add_reg_plane(x = mtcars$mpg, y = mtcars$hp, z = mtcars$drat, plot = testPlot)

  my_result <- testReg$x$source

  expect_equal(my_result, correct_result)

})


test_that('theme_ly', {

  correct_result <- 'A'

  testPlot <- easy_plot_ly(x = mtcars$mpg, y = mtcars$hp, z = mtcars$drat, type = 'scatter',
                           color = mtcars$cyl, data = mtcars)

  testTheme <- theme_ly(xlab = 'MPG', y = 'HP', z = 'Rear Axle Ratio', title = 'Cars Data',
                        plot = testPlot)

  my_result <- testTheme$x$source

  expect_equal(my_result, correct_result)
})
