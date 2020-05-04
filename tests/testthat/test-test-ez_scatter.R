test_that("ez_scatter function works", {

  correct_plot1 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point())

  my_plot1 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num = "Sepal.Length"))

  assert(my_plot1, correct_plot1)

  correct_plot2 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(shape = 3))

  my_plot2 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = 3))

  assert(my_plot2, correct_plot2)

  correct_plot3 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(color = "steelblue", shape = 3))

  my_plot3 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", color = "steelblue", shape = 3))

  assert(my_plot3, correct_plot3)

  correct_plot4 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species")) +
    scale_colour_brewer(palette = "Dark2"))

  my_plot4 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2"))

  assert(my_plot4, correct_plot4)

  correct_plot5 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(shape = shape)))

  my_plot5 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = "Species"))

  assert(my_plot5, correct_plot5)

  correct_plot6 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species", shape = "Species")) +
    scale_colour_brewer(palette = "Dark2"))

  my_plot6 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2", shape = "Species"))

  assert(my_plot6, correct_plot6)

})
