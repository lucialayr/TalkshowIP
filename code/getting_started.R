setwd("~//dataviz/TalkshowIP")

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

df = read_csv("code/shiny/data/Daten.csv")

runApp("code/shiny")

