# install.packages("shiny")
library("shiny")

# Lesson 1
runExample("01_hello") # Old Faithful adjustable histogram bins 

runApp("App-1", display.mode = "showcase")

runApp("census-app", display.mode = "showcase")

counties <- readRDS("census-app/data/counties.rds")
head(counties)

install.packages(c("maps", "mapproj"))

library(maps)
library(mapproj)
source("census-app/helpers.R")
counties <- readRDS("census-app/data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

install.packages("quantmod")
runApp("stockVis")
