#' A shortcut for generating plots with plot_ly
#'
#' @usage ## x, y, z, and color variables need to be in a vectorized form like x$data or y$data.
#' ## If you are using the surface plot type, a z variable matrix is all that should be used,
#' ## if non matrix data is put into the funciton for x and y they will be converted to a z matrix
#' ## Line plots should use quantitative color variables. 3D density plots require only an x and y variable
#' ## and will create a z matrix for you
#'
#' easy_plot_ly(x, y, z, color, type, data)
#'
#' @param x The x-axis variable
#' @param y The y-axis variable
#' @param z The z-axis variable
#' @param color A cateogrical or quantitative variable to subset the data
#' @param type The type of plot you would like to generate: "scatter", "line",
#' "surface", "3d_density", "mesh", or "auto" to have plotly generate one for you
#' @param data The datset that will be used
#'
#' @examples
#'            Scatter Plot:
#' easy_plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#'              color = iris$Species, type = 'scatter', data = iris)
#'
#'
#'            Line Plot:
#' df1 <- data.frame(x = sin(1:1000), y = cos(1:1000), z = 1:1000)
#' easy_plot_ly(x = df1$x, y = df1$y, z = df1$z, color = df1$x, type = 'line', data = df1)
#'
#'
#'            Surface Plot:
#' easy_plot_ly(z = volcano, type = 'surface', data = volcano)
#'
#'
#'            3D Density Plot:
#' easy_plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, type = '3d_density', data = iris)
#'
#'
#'            Mesh Plot:
#' easy_plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#'              type = 'mesh', data = iris)
#'
#'
#' @return A 2D or 3D plot based on the package plotly
#'
#' @import plotly
#' @import tidyverse
#' @import viridis
#' @import reshape2
#' @import MASS
#'
#' @export

easy_plot_ly <- function(x = NULL, y = NULL, z = NULL, color = NULL, type = "auto", data = NULL){

  if(type == 'scatter'){
    plot_ly(data, x = x, y = y, z = z, color = color,
            colors = viridis_pal(alpha = 0.2, option = "D")(3)) %>%
      add_markers()

  }

  else if(type == 'line'){
    plot_ly(data, x = x, y = y, z = z, type = 'scatter3d', mode = 'lines',
            line = list(width = 5, color = color, colorscale = 'Viridis'))
  }

  else if(type == 'surface'){

    if(class(x) != 'matrix' & class(y) != 'matrix' & class(z) != 'matrix'){
      z = matrix(x, y)

      plot_ly(z = z, type = 'surface') %>% add_surface( contours = list(
        z = list(show=TRUE, usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE)))) %>%
        hide_colorbar()
    }

    else {
      plot_ly(z = z, type = 'surface') %>% add_surface( contours = list(
        z = list(show=TRUE, usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE)))) %>%
        hide_colorbar()
    }
  }

  else if(type == '3d_density'){
    kd <- kde2d(x = x, y = y, n =50)
    plot_ly(x = kd[[1]], y = kd[[2]], z = kd[[3]]) %>% add_surface()
  }

  else if(type == 'mesh'){
    plot_ly(data, x = x, y = y , z = z, type = 'mesh3d', opacity=0.7)
  }

  else {

    if(is.null(z) == T){
      plot_ly(data, x = x, y = y, color = color)
    }

    else {
    plot_ly(data, x = x, y = y, z = z, color = color)
    }
  }
}

#' Adds a 3D regression plane to a plot.
#'
#' @usage Recommended that you use this function with a scatter plot.
#'
#' add_reg_plane(x, y, z , plot, color = 'blue')
#'
#' @param x The x-axis variable
#' @param y The y-axis variable
#' @param z The z-axis variable
#' @param plot The plot that you want to add a plane to
#' @param color The color of the plane
#'
#' @examples
#' newPlot <- easy_plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#'              color = iris$Species, type = 'scatter', data = iris)
#' add_reg_plane(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length, plot = newPlot)
#'
#'
#' @return A 3D plane added to an exisiting plot
#'
#' @export

add_reg_plane <- function(x, y, z, plot, color = 'blue'){

  templm <- lm(z ~ 0 + x + y)

  graph_reso <- 0.05

  axis_x <- seq(min(x), max(x), by = graph_reso)
  axis_y <- seq(min(y), max(y), by = graph_reso)

  tempSurface <- expand.grid(x = axis_x, y = axis_y,KEEP.OUT.ATTRS = FALSE)
  tempSurface$z <- predict.lm(templm, newdata = tempSurface)
  tempSurface <- acast(tempSurface, y ~ x, value.var = 'z')

  add_trace(p = plot, z = tempSurface, x = axis_x, y = axis_y,
            type = "surface", colorscale = list(c(0, 1), c("tan", color))) %>%
    layout(legend = list(x = 0.01, y = 0.9))

}

#' Edits plot aesthetics
#'
#' @usage
#'
#' theme_ly(xlab, ylab, zlab, title, ...)
#'
#' @param xlab The x-axis label
#' @param ylab The y-axis label
#' @param zlab The z-axis label
#' @param title The plot title
#' @param colorscale_title The title for the colorscale
#' @param legend Show legend
#' @param colorbar Show colorbar
#' @param plot The plot that you want to change the aesthetics of
#'
#' @examples
#' newPlot <- easy_plot_ly(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#'              color = iris$Species, type = 'scatter', data = iris)
#'
#' newPlot2 <- add_reg_plane(x = iris$Sepal.Length, y = iris$Sepal.Width, z = iris$Petal.Length,
#' plot = newPlot)
#'
#' theme_ly(xlab = 'Sepal Length', ylab = 'Sepal Width', zlab = 'Petal Length', title = 'Regression',
#' legend = F, plot = newPlot2, colorbar = FALSE)
#'
#' @return aesthetics adjustments for a plot
#'
#' @export

theme_ly <- function(xlab = NULL, ylab = NULL, zlab = NULL, title = NULL, colorbar = FALSE,
                    colorscale_title = NULL, legend = FALSE, plot){

  if(colorbar == FALSE){
  plot %>% layout(title = title,
                  scene = list(xaxis = list(title = xlab),
                               yaxis = list(title = ylab),
                               zaxis = list(title = zlab))) %>%
    hide_colorbar() %>% layout(showlegend = legend)
  }

  else {
  plot %>% layout(title = title,
                  scene = list(xaxis = list(title = xlab),
                               yaxis = list(title = ylab),
                               zaxis = list(title = zlab))) %>%
      layout(showlegend = legend)
  }
}

