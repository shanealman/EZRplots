#' A shortcut for generating plots with plot_ly
#'
#' @param x The x-axis variable
#' @param y The y-axis variable
#' @param z The z-axis variable
#' @param color A cateogrical or quantitative varaible to subset the data
#' @param type The type of plot you would like to generate: scatter, line,
#' surface - only takes a z-axis matrix, 3d_density, mesh, or specify nothing to
#' autogenerate a plot
#' @param data The datset that will be used
#'
#' @return A plot
#'
#' @import plotly
#' @import tidyverse
#' @import viridis
#' @import reshape2
#'
#' @export

easy_plot_ly <- function(data, x, y, z, color, type = ...){

  if(type == 'scatter'){

    plot_ly(data, x = x, y = y, z = z, color = color,
            colors = viridis_pal(alpha = 0.5, option = "D")(3)) %>%
      add_markers()

  }

  else if(type == 'line'){
    plot_ly(data, x = x, y = y, z = z, type = 'scatter3d', mode = 'lines',
            line = list(width = 8, color = color, colorscale = 'Viridis'))
  }

  else if(type == 'surface'){

    plot_ly(z = z, type = 'surface') %>% add_surface( contours = list(
      z = list(show=TRUE, usecolormap=TRUE, highlightcolor="#ff0000", project=list(z=TRUE)))) %>%
      hide_colorbar()
  }

  else if(type == '3d_density'){

    kd <- kde2d(x = x, y = y, n =50)
    plot_ly(x = kd[[1]], y = kd[[2]], z = kd[[3]]) %>% add_surface()
  }

  else if(type == 'mesh'){

    plot_ly(data, x = x, y = y , z = z, type = 'mesh3d')
  }

  else {

    plot_ly(data, x = x, y = y, z = z, color = color)
  }
}

#' Adds a 3D regression plane to a plot. It is recommended that you use this with a scatter plot
#'
#' @param x The x-axis variable
#' @param y The y-axis variable
#' @param z The z-axis variable
#' @param plot The plot that you want to add a plane to
#' @param color The color of the plane
#'
#' @return A 3D plane
#'
#' @export

add_reg_plane <- function(plot, x, y, z, color = 'pink'){

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
#' @param x_lab The x-axis label. A string
#' @param y_lab The y-axis label. A string
#' @param z_lab The z-axis label. A string
#' @param title The plot title. A string
#' @param colorscale_title The title for the colorscale. A string
#' @param legend Show legend. TRUE or FALSE. Defualt is FALSE.
#' @param plot The plot that you want to add a plane to
#'
#' @return aesthetics adjustments for a plot
#'
#' @export

theme_ly <- function(plot, x_lab, y_lab, z_lab, title,
                     colorscale_title, legend = FALSE){

  plot %>% layout(title = title, scene = list(xaxis = list(title = x_axis),
                                              yaxis = list(title = y_axis), zaxis = list(title = z_axis))) %>%
    colorbar(title = colorscale_title) %>% layout(showlegend = legend)

}

