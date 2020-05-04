test_that("ez_scatter function works", {

  correct_plot1 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point()

  my_plot1 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length")

  expect_equal(my_plot1, correct_plot1)

  correct_plot2 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(shape = 23)

  my_plot2 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = 23)

  expect_equal(my_plot2, correct_plot2)

  correct_plot3 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(color = "steelblue", shape = 23)

  my_plot3 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", color = "steelblue", shape = 23)

  expect_equal(my_plot3, correct_plot3)

  correct_plot4 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species")) +
    scale_colour_brewer(palette = "Dark2")

  my_plot4 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2")

  expect_equal(my_plot4, correct_plot4)

  correct_plot5 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(shape = shape))

  my_plot5 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = "Species")

  expect_equal(my_plot5, correct_plot5)

  correct_plot5 <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species", shape = "Species")) +
    scale_colour_brewer(palette = "Dark2")

  my_plot5 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2", shape = "Species")

  expect_equal(my_plot5, correct_plot5)

})
