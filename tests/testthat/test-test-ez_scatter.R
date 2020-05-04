test_that("ez_scatter function works", {

  correct_result1 <- ggplot(data, aes_string(x = x_num, y = y_num)) +
    geom_point()

  my_plot1 <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2", shape = "Species")


















  correct_result <- ggplot(iris, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
    geom_point(aes_string(color = "Species", shape = "Species")) +
    scale_colour_brewer(palette = "Dark2")

  my_plot <- ez_scatter(data = iris, x_num = "Sepal.Width", y_num =  "Sepal.Length", z_cat = "Species", color = "Dark2", shape = "Species")

  expect_equal(my_result, correct_result)

})





#data, x_num, y_num, z_cat = NULL, color = NULL, shape = NULL
