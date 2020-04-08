# analysis-employee-data

These R Shiny scripts use pivot_longer to group employee data in 2 semi-long formats as the data for salary and commuting have different factor levels.
Also, different tidyverse techniques are used to create contingency tables for calculating statistics and graphing plots.
Different plotting functions are used including ggplot2, corrplot, and base plot.

The plots are displayed on a tabbed summary page in R Shiny. 

R Shiny reactive elements are included for the corrplot plots to allow for custom data visualisations. 

### The code is written in R. 

## Dependencies

The dependencies are:

* RStudio Version 1.1.456 
* tidyverse
* shiny
* broom
* corrplot
* gplots
* caTools
* ROCR
* plotROC
