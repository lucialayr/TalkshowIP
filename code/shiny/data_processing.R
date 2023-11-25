setwd("~/03_Outreach/dataviz/Talkshow/code/shiny")

library(tidyverse)
library(scico)


df = read_csv("data/Daten.csv")


variable = "Nationalität"


plot1 = function(variable = "Nationalität", df=df) {
  df1 = df %>%
    group_by(!!rlang::sym(variable)) %>%
    count() 
  
  (p1 = ggplot() + theme_void() +
      geom_bar(data = df1, alpha = .5, color = "black", stat = "identity",
               aes(x = !!rlang::sym(variable), y =n,  fill = !!rlang::sym(variable))) +
      scico::scale_fill_scico_d(palette = "batlow") +
      theme(axis.text.x = element_text(size = 15, angle = 90),
            legend.position = "None"
      ))
  
  return(p)
}

