setwd("~/03_Outreach/dataviz/Talkshow")

library(shiny)
library(tidyverse)

df = read_csv("code/shiny/data/Daten.csv")

runApp("code/shiny")
