#' A shortcut for generating multiple plots side by side using the package patchwork
#'
#' @param p1 The first plot
#' @param p2 The second plot
#' @param p3 The third plot
#' @param p4 The fourth plot
#' @param ncol The dimensions of the grid to create
#' @param nrow The dimensions of the grid to create
#' @param byrow If FALSE the plots will be filled in in column-major order
#' @param widths The widths of each row and column in the grid
#' @param heights The heights of each row and column in the grid
#' @param guides A string specifying how guides should be treated in the layout
#' @param tag_level A string ('keep' or 'new') to indicate how auto-tagging should behave
#' @param design Specification of the location of areas in the layout
#'
#' mult_plots(p1,p2,p3,p4, ncol, nrow, byrow, widths, guides, tag_level, design)
#'
#' @return Multiple plots side by side
#'
#' @import tidyverse
#' @import patchwork
#' @import english
#'
#' @export

mult_plots <- function(p1 = NULL, p2 = NULL, p3 = NULL, p4 = NULL, ncol = NULL, nrow = NULL, byrow = NULL,
                       widths = NULL, heights = NULL, guides = NULL, tag_level = NULL, design = NULL) {


p1 + p2 + p3 + p4


}
