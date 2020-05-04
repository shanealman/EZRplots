data("countries")
data("us_states")
test_that("Leaflet works", {

  my_plot <- leafletPlot(us_states@data, "name", "density", "us_states", bins_of_map = c(0,10,50,100,500,Inf))

  expect_equal(class(my_plot), c("leaflet", "htmlwidget"))

})


