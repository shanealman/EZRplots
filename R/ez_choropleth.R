#' A shortcut for generating choropleth maps
#'
#' @param data_set The name of the data set
#' @param col_name The column name in the data set containing the regions (must be in quotes)
#' @param col_variable The column name containing the variable of interest (must be in quotes)
#' @param type_of_map The type of map you wish to specify; possible options are "countries", "us_states"
#' @param bins_of_map The type of bins user wishes to use for map; must be a vector
#' @param col_variable_unit The units of your variable (must be in quotes); if it is a percentage, put this argument as "%"
#' @param title The title of the chloropleth map (must be in quotes)
#'
#' @return Leaflet Map
#'
#' @import leaflet
#' @importFrom dplyr rename
#'
#' @export
leafletPlot <- function(data_set, col_name, col_variable, type_of_map, bins_of_map, col_variable_unit = "", title = "Choropleth Map"){

  data("countries")
  data("us_states")

  #renaming the user input variable names
  data_set <- data_set %>%
    rename(
      RegionName = col_name,
      VariableMap = col_variable
    )

  #if the map desired if countries
  if (type_of_map == "countries"){
    data_set_map <- countries %>%
      sp::merge(data_set, by.x = "ADMIN", by.y= "RegionName")
    ViewLong <- 14
    ViewLat <- 0
    ViewSet <- 2

    #initializing the labels
    if(col_variable_unit == "%"){
      labels <- paste(data_set_map$ADMIN,": ", paste(round(data_set_map$VariableMap*100, digits = 2), "%", sep = ""))
      labFormatSave <- labelFormat(suffix = "%", transform = function(x) x*100)
    }
    #if the labels are not percentages
    else{
      labels <- paste(data_set_map$ADMIN,": ", paste(round(data_set_map$VariableMap, digits = 2), col_variable_unit, sep = " "))
      labFormatSave <- labelFormat(suffix = paste(" ",col_variable_unit))

    }
  }
  #if the map specified is us states
  else if (type_of_map == "us_states"){
    data_set_map <- us_states %>%
      sp::merge(data_set, by.x = "name", by.y= "RegionName")
    ViewLong <- -96
    ViewLat <- 37.8
    ViewSet <- 4

    #initializing the labels
    if(col_variable_unit == "%"){
      labels <- paste(data_set_map$name,":", paste(round(data_set_map$VariableMap*100, digits = 2), "%", sep = ""))
      labFormatSave <- labelFormat(suffix = "%", transform = function(x) x*100)
    }

    #if the labels are not percentages
    else{
      labels <- paste(data_set_map$name, ": ", paste(round(data_set_map$VariableMap, digits = 2)," ", col_variable_unit, sep = ""))
      labFormatSave <- labelFormat(suffix = paste(" ",col_variable_unit))
    }

  }

  #creating bins to put on chloropleth map
  pal <- colorBin("YlOrRd", domain = data_set_map$VariableMap, bins = bins_of_map)

  map <- make_map(data_set_map, ViewLong, ViewLat, ViewSet, labels, labFormatSave, title, pal)

  return(map)

}
#' Helper function to make the choropleth map
#'
#' @param data_set_map The name of the data set
#' @param ViewLong The longitude to set the view of the map
#' @param ViewLat The latitude to set the view of the map
#' @param ViewSet The required zoom of the map
#' @param labels The labels to use in the map
#' @param labFormatSave The formatting of the map
#' @param title The title of the chloropleth map (must be in quotes)
#' @param pal The bins that will be displayed on the map
#'
#' @return Leaflet Map
make_map <- function(data_set_map, ViewLong, ViewLat, ViewSet, labels, labFormatSave, title, pal){
  map <- leaflet(data_set_map) %>%
    setView(ViewLong, ViewLat, ViewSet) %>%
    addTiles() %>%
    #adding the percentages data in
    addPolygons(
      fillColor = ~pal(VariableMap),
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
    addControl(title, position = "topright")%>%
    #adding the legend
    addLegend(pal = pal, values = ~VariableMap, opacity = 0.7, title = NULL,
              position = "bottomright", labFormat = labFormatSave)

  return(map)

}


