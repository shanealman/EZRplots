#' A shortcut for generating multiple plots side by side using the package patchwork
#'
#' @param p1 The first plot
#' @param p2 The second plot
#' @param p3 The third plot
#' @param p4 The fourth plot
#'
#'mult_plots(p1, p2, p3, p4)
#'
#' @return Multiple plots side by side
#'
#' @import tidyverse
#' @import patchwork
#' @import english
#' @import ggplot2
#'
#' @export

mult_plots <- function(p1 = NULL, p2 = NULL, p3 = NULL, p4 = NULL) {


return(p1 + p2 + p3 + p4)
}
