library(tidyverse)
library(leaflet)
data("countries")
data("us_states")
test_that("Leaflet works", {

  my_plot <- leafletPlot(us_states@data, "name", "density", "us_states", bins_of_map = c(0,10,50,100,500,Inf))

  my_result <- typeof(my_plot)
  labels <- paste(us_states$name, ": ", paste(round(us_states$density, digits = 2)," ", "", sep = ""))
  labFormatSave <- labelFormat(suffix = paste(" ",""))
  pal <- colorBin("YlOrRd", domain = us_states$density, bins = c(0,10,50,100,500,Inf))

  other_plot <- leaflet(us_states) %>%
    setView(-96, 37.8, 4) %>%
    addTiles() %>%
    #adding the percentages data in
    addPolygons(
      fillColor = ~pal(us_states$density),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      #styling options
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    #adding title
    addControl("Choropleth Map", position = "topright")%>%
    #adding the legend
    addLegend(pal = pal, values = ~us_states$density, opacity = 0.7, title = NULL,
              position = "bottomright", labFormat = labFormatSave)

    correct_result <- typeof(other_plot)
    expect_equal(my_result, correct_result)


})


