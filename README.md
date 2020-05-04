# EZRplots
 A package that provides shortcuts for making elegant plots

## Installation

You can install the the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shanealman/EZRplots")
```

```{r}
library(EZRplots)
```

## leafletPlot
The first function will create a chloropleth map for the user. The user will input the data set they wish to use for the map, which will have 2 columns: the names of their designated region and the variable of interest. The user will then specify which map they wish to display: whether it is all the countries, the states, global regions, etc. The user will have the option to specify additional parameters stating the units of their variable, how many bins for the legend they wish to create, etc. The function will return the resulting cloropleth map altered to the users' desires.

## easy_plot_ly
This is a shortcut function to help generate 2D or 3D plots with plot_ly. The user will input their data and the variables they want for the x, y, and z axes. The user will be able to specify the type of plot they want to generate from several basic options like scatter, line, surface, 3d_density, mesh, or an autogenerated plot. Addtional helper functions are available to add a 3D regression plane and to change the plot aesthetics.

## mult_plots
This is a function that generates multiple plots of different types. The function uses the package patchwork to make it easy for the user to display multiple plot types at one time. The user will first create the plots they want using the package ggplot2.The user will then input the synatax of the two or more graphs they want to display side by side into the function each as an argument. 


## ez_scatter

This is a function that can produce a variety of scatterplots from ggplot2 depending on the inputs that the user chooses to include in the function. Users may input different datasets, variables, colors, and shapes to customize their graphs. For variable names and colorscusers will need to input their arguments as stings to allow ggplot to work properly within the function. It is also important to note that functions with only two variables should have colors inputed corresponding to those in ggplot2 and the shape argument should be a number corressponding to the point shapes that ggplot2 provides. For functions with three variables you may enter colors from RColorBrewer and the shape argument must be a string of the categorical varibles.

Example with two variables.

```{r}
ez_scatter(data = iris, x_num = "Petal.Width", y_num = "Petal.Length", color = "steelblue", shape = 3)
```
Example with three variables.

```{r}
ez_scatter(data = iris, x_num = "Petal.Width", y_num = "Petal.Length", z_cat = "Species", color = "Spectral", shape = "Species")
```
### ez_labels

This function can be added to an existing scatterplot in order to add a title to the plot, add axis labels, and customize the colors and typography of those labels.

```{r}
plot <- ez_scatter(data = iris, x_num = "Petal.Width", y_num = "Petal.Length", z_cat = "Species", color = "Blues", shape = "Species")

ez_labels(plot = plot, title = "Petal Width vs Petal Length", x = "Petal Width", y = "Petal Length", title_color = "green", axis_color = "orange", title_type = "bold.italic", axis_type = "bold")
```

