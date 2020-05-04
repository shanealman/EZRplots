test_that("ez_scatter function output matches type", {

  correct_plot1 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point())

  my_plot1 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num = "Sepal.Length"))

  expect_equal(my_plot1, correct_plot1)

  correct_plot2 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(shape = 3))

  my_plot2 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = 3))

  expect_equal(my_plot2, correct_plot2)

  correct_plot3 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(color = "steelblue", shape = 3))

  my_plot3 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", color = "steelblue", shape = 3))

  expect_equal(my_plot3, correct_plot3)

  correct_plot4 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species")) +
    scale_colour_brewer(palette = "Dark2"))

  my_plot4 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2"))

  expect_equal(my_plot4, correct_plot4)

  correct_plot5 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(shape = "Species")))

  my_plot5 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", shape = "Species"))

  expect_equal(my_plot5, correct_plot5)

  correct_plot6 <- typeof(ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species", shape = "Species")) +
    scale_colour_brewer(palette = "Dark2"))

  my_plot6 <- typeof(ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2", shape = "Species"))

  expect_equal(my_plot6, correct_plot6)

})

test_that("ez_labels function output matches type", {

  plot <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point()

  correct_graph <- typeof(plot +
    labs( title = "Sepal Width vs Length", x = "Sepal.Width", y = "Sepal.Length") +
    theme(
      plot.title = element_text(color = "Red", size = 20, face = "bold.italic"),
      axis.title.x = element_text(color = "black", size = 14, face = "bold"),
      axis.title.y = element_text(color = "black", size = 14, face = "bold")))

  my_graph <- typeof(ez_labels(plot = plot, title = "Sepal Width vs Length", x = "Sepal.Width", y = "Sepal.Length", title_color = "Red", axis_color = "black", title_type = "bold.italic", axis_type = "bold"))

  expect_equal(corect_graph, my_graph)

})
