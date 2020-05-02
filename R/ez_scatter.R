#' Shortcut functions for generating scatterplots from ggplot2
#'
#' @param x_num numeric variable for x-axis
#' @param y_num numeric variable for y-axis
#' @param z_cat optional categorical variable for different groups
#' @param color color of the z_cat variable
#' Defaults to `TRUE`.
#'
#' @return A plot
#'
#' @import ggplot2
#'
#' @export

ez_scatter <- function(data, x_num, y_num, z_cat = NULL, color = NULL) {

  if(is.null(color) == TRUE) {

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat))

  }

  else if(color == "Red"){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = "Reds")

  }

  else if(color == "Orange"){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = "Oranges")
  }

  else if(color == "Green"){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = "Greens")
  }

  else if(color == "Blue"){
    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = "Blues")
  }

  else if(color == "Purple"){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = "Purples")
  }

  else if(color == color){

    ggplot(data, aes_string(x = x_num, y = y_num)) +
      geom_point(aes_string(color = z_cat)) +
      scale_colour_brewer(palette = color)
  }
}

