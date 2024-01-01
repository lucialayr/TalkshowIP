install.packages("tidyverse")
install.packages("shiny")
install.packages("plotly")
install.packages("shinythemes")
install.packages("scico")

library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(scico)

setwd("~//dataviz/TalkshowIP")

df = read_csv("shiny/data/data.csv")

runApp("shiny")

