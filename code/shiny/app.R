library(shiny)
library(shinythemes)
library(tidyverse)
library(scico)
library(lubridate)
library(plotly)


df = read_csv("data/Daten.csv")

plot1 = function(variable, df=df) {
  df1 = df %>%
    group_by(!!rlang::sym(variable)) %>%
    count() 
  
  p1 = ggplot() + theme_void() +
      geom_bar(data = df1, alpha = .5, color = "black", stat = "identity",
               aes(x = !!rlang::sym(variable), y =n,  fill = !!rlang::sym(variable))) +
      scico::scale_fill_scico_d(palette = "batlow", direction = 1) +
      theme(axis.text.x = element_text(size = 15, angle = 90),
            axis.text.y = element_text(size = 15),
            legend.position = "None"
      )
  
  return(p1)
}

plot2 = function(variable1, variable2, df) {
  
  df = df %>%
    group_by(across(c(-Datum, -Titel, -Sendung))) %>%
    count() %>%
    mutate(n = as.integer(n))
  
  
  (p2 = ggplot() + theme_void() + coord_equal() +
    geom_line(data = df, position = position_jitter(width = .5, height = .5, seed = 123), linewidth = .75, alpha = .9, color = "black",
               aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2), group = interaction(Name))) +
      geom_point(data = df, shape = 21,  position = position_jitter(width = .5, height = .5, seed = 123),  alpha = .9,
                 aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2),  fill = !!rlang::sym(variable1), size = n)) +
      scale_size_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0,max(df$n))) +
     scico::scale_fill_scico_d(palette = "batlow", direction = 1) +
      scico::scale_color_scico_d(palette = "batlow", direction = 1) +
     theme(axis.text.x = element_text(size = 15, angle = 90),
           axis.text.y = element_text(size = 15),
           legend.position = "None"
     ))
  
   return(p2)
}

plot3 = function(start = "2023-10-07", end = "2023-10-30", df) {
  
  print(start)
  
  df = df %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%y")) %>%
    filter(Datum >= as.Date(start), Datum <= as.Date(end)) %>%
    mutate(Week = lubridate::week(Datum),
           Sunday = Datum - lubridate::wday(Datum) + 1) %>%
    group_by(Week, Sunday, Datum) %>%
    count() %>%
    group_by(Week, Sunday) %>%
    count()
  
  ggplot() + theme_void() +
    geom_line(data = df, aes(x = Sunday + 1, y = n)) +
    geom_point(data = df, fill = "grey50", color = "black", shape = 21, position = "identity",
               aes(x = Sunday + 1, y = n, size = n)) +
    scale_y_continuous(limits = c(0, max(df$n))) +
    scale_size_continuous(limits = c(1, max(df$n)), range = c(1, max(df$n)*2)) +
    theme(axis.text.x = element_text(size = 15, angle = 90),
          axis.text.y = element_text(size = 15),
          axis.title = element_blank(),
          legend.position = "None"
    )
  
  
    #get date in correct format
  #select date range
  #plot plot

  
  
  
}



# Define UI ----
ui <- fluidPage(theme = shinytheme("flatly"),
  
  titlePanel("Wer sitzt in deutschen Talkshows zum Nahost-Konflikt?"),
  
  
  sidebarLayout(
    sidebarPanel(selectInput("p1_input", h4("Kategorie Auswählen"), 
                             choices = list("Profession", "Nationalität",
                                            "Perspektive_Identität", "Geschlecht"), selected = 1)),
    mainPanel(h3(textOutput("t1")),
              plotOutput("p1"))
  ),
  
  sidebarLayout(
    sidebarPanel(selectInput("p2_input1", h4("Kategorie Auswählen"), 
                             choices = list("Profession", "Nationalität",
                                            "Perspektive_Identität", "Geschlecht"), selected = 1),
                 selectInput("p2_input2", h4("Kategorie Auswählen"), 
                             choices = list("Profession", "Nationalität",
                                            "Perspektive_Identität", "Geschlecht"), selected = 1)),
    mainPanel(h3(textOutput("t2")),
              plotOutput("p2"))
  ),
  
  sidebarLayout(
    sidebarPanel(dateRangeInput("p3_input", h4("Zeitraum auswählen"), format = "yyyy-mm-dd", start  = "2023-10-07", min =  "2023-10-07")),
    mainPanel(h3(textOutput("t3")), 
              plotOutput("p3"))
  )
  
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$t1 = renderText(paste0(input$p1_input, " der Gäste"))
  output$p1 = renderPlot({
    plot1(variable = input$p1_input, df)
  })
  
  output$t2 = renderText(paste0(input$p2_input1, " vs. ", input$p2_input2,  " der Gäste"))
  output$p2 = renderPlot({
    plot2(variable1 = input$p2_input1, variable2 = input$p2_input2,  df)
  })
  
  
  output$t3 = renderText(paste0("Anzahl der Runden pro Woche"))
  output$p3 = renderPlot({
    plot3(start= input$p3_input[1], end = input$p3_input[2],  df)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
