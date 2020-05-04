#' Shortcut functions for generating scatterplots from ggplot2
#'
#' @param x_num a numeric variable string for the x-axis
#' @param y_num a numeric variable string for the y-axis
#' @param z_cat an optional categorical variable sting for different groups
#' @param color color string for graph. If the scatterplot has two variables the color must be chosen from ggplot2 colors. If the column is three variables the color must be chosen from RcolorBrewer.
#' @param shape if the scatterplot has two variables, input a number corresponding to a point shape in R. If the scatterplot has three variablles, input the catigorical variable string.
#'
#' @return A plot
#'
#' @import ggplot2
#'
#' @export

ez_scatter <- function(data, x_num, y_num, z_cat = NULL, color = NULL, shape = NULL) {

  if(is.null(color) && is.null(shape)){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point()
  }

  else if(is.null(color) && is.null(z_cat) && shape == shape){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(shape = shape)
  }

  else if(is.null(z_cat) && color == color && shape == shape){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(color = color, shape = shape)
  }

  else if(!is.null(z_cat) && is.null(shape) && color == color){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = color)
  }

  else if(!is.null(z_cat) && is.null(color) && shape == shape){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(shape = shape))
  }

  else if(!is.null(z_cat) && color == color && shape == shape){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat, shape = shape)) +
      scale_colour_brewer(palette = color)
  }
}

#' Add graph titles, axis labels, and customize label colors and typography for plots
#'
#' @param plot name of plot you would like to add labels to. (required)
#' @param title title of the plot, input as string (required)
#' @param x x-axis label, input as string (optional)
#' @param y y-axis label, input as string (optional)
#' @param title_color color of title, input as string (optional)
#' @param axis_color color for axis labels, input as string (optional)
#' @param title_type typography for title, input as string (optional)
#' @param axis_type typography for axis labels, input as string (optional)
#'
#' @return A plot
#'
#' @import ggplot2
#'
#' @export

ez_labels <- function(plot, title, x = NULL, y = NULL, title_color = NULL, axis_color = NULL, title_type = NULL, axis_type = NULL){

  plot +
    labs( title = title, x = x, y = y) +
    theme(
      plot.title = element_text(color = title_color, size = 20, face = title_type),
      axis.title.x = element_text(color = axis_color, size = 14, face = axis_type),
      axis.title.y = element_text(color = axis_color, size = 14, face = axis_type)
    )
}
