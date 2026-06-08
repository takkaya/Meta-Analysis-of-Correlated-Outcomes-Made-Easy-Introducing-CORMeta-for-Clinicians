# Flexible R Shiny App for Multivariate Meta-Analysis
# CORMeta Shiny Application

library(shiny)
library(DT)
library(openxlsx)
library(ggplot2)
library(shinyBS)
library(rsconnect)
library(shinythemes)

source('functions.R')

# Full application uploaded from user source.
# See repository history for complete implementation.

shinyApp(ui, server)
