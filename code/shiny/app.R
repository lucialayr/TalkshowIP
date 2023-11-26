library(shiny)
library(shinythemes)
library(tidyverse)
library(scico)
library(lubridate)
library(plotly)


df = read_csv("data/Daten.csv")


cutoff = function(df, variable, thresh = 1) {
  df0 = df %>%
    group_by(!!rlang::sym(variable)) %>%
    count() %>%
    mutate(indicator = case_when(!!rlang::sym(variable) %in% c("deutsch", "israelisch", 
                                                               "jüdisch", "palestinänsisch") ~ !!rlang::sym(variable),
                                n > thresh ~ !!rlang::sym(variable),
                                 TRUE ~ "andere")) 
  return(df0)
  
}

add_plot_layout = function(fontsize = 15, background = '#f9f6ee') {
  theme_classic() %+replace%
    theme(axis.title = element_text(size = fontsize),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill=background, color = NA),
          legend.box.background = element_rect(fill=background, color = NA),
          panel.background = element_rect(fill = background, colour = NA),  
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.background = element_rect(fill = background, colour = NA),
          strip.background = element_rect(fill = background, color = NA),
          strip.text = element_text(size = fontsize),
          text = element_text(size = fontsize))
}

plot1 = function(variable, df=df, thresh = 1) {
  df0 = cutoff(df, variable, thresh)
  
  label_andere = df0 %>%
    filter(n <= thresh) %>%
    mutate(label = paste(!!rlang::sym(variable), ": ", n, " Auftritte<br>")) %>%
    ungroup() 
  
  label = paste(label_andere$label, collapse = " ")
  
  df1 = full_join(df, df0) %>%
    group_by(indicator) %>%
    count() %>%
    mutate(text = if_else(indicator == "andere", as.character(label), paste0(as.character(indicator), ": ", n, " Auftritte")))
  
  p1 = ggplot() + theme_void() +
      geom_bar(data = df1, alpha = 1, color = "black", stat = "identity", width = .5,
               aes(x = indicator, y =n,  fill = indicator, text = text)) +
      scico::scale_fill_scico_d(palette = "batlow", direction = 1) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(name = "") + 
    add_plot_layout(fontsize = 15) +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.line.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey80"),
            legend.position = "None"
      )
  
  return(p1)
}

plot2 = function(variable1, variable2, df) {
  
  df01 = cutoff(df, variable1)
  
  andere1 = df01[df01$indicator == "andere", ][[variable1]]
  
  df02 = cutoff(df, variable2)
  
  andere2 = df02[df02$indicator == "andere", ][[variable2]]
  
  df2 = df %>%
    mutate(!!rlang::sym(variable1) := if_else(!!rlang::sym(variable1) %in% andere1, 
                                            "andere", !!rlang::sym(variable1)),
           !!rlang::sym(variable2) := if_else(!!rlang::sym(variable2) %in% andere2, 
                                              "andere", !!rlang::sym(variable2))) %>%
    group_by(across(c(-Datum, -Titel, -Sendung))) %>%
    count() %>%
    mutate(n = as.integer(n))
  
  
  (p2 = ggplot() + theme_void() + coord_equal() +
    geom_tile(data = df2, aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2), fill = NA, colour = 'black'), linetyp = "dotted") +
    geom_line(data = df2, position = position_jitter(width = .5, height = .5, seed = 123), linewidth = .3, alpha = .9, color = "grey20",
               aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2), group = interaction(Name))) +
    geom_point(data = df2, shape = 21,  position = position_jitter(width = .5, height = .5, seed = 123),  alpha = .9,
                 aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2),  fill = !!rlang::sym(variable1), size = n)) +
    scale_size_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0,max(df2$n))) +
    scico::scale_fill_scico_d(palette = "batlow", direction = 1) +
    scico::scale_color_scico_d(palette = "batlow", direction = 1) +
      scale_x_discrete(expand = c(0,0)) +
    add_plot_layout(fontsize = 15) +
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
    add_plot_layout(fontsize = 15) +
    scale_y_continuous(limits = c(0, max(df$n)), expand = c(0,.5)) +
    scale_size_continuous(limits = c(1, max(df$n)), range = c(1, max(df$n)*2)) +
    theme(axis.text.x = element_text(size = 15, angle = 90),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.y = element_line(color = "grey80"),
          legend.position = "None"
    )
  
  
    #get date in correct format
  #select date range
  #plot plot

  
  
  
}



# Define UI ----
ui <- fluidPage(#theme = shinytheme("flatly"),
                tags$head(
                  tags$style(HTML(paste0("body {
                                   background-color: #f9f6ee; /* Set your desired background color */
                                   }
                                    #p1_input, .selectize-control.single .selectize-input {
                                    background-color: #f5fffa;
                                    }
                                    .well {
                                    border: .5px solid black;
                                    border-radius: 2px;
                                    border-shadow: none;
                                    }
      }
    ")))),
                  
    titlePanel("Wer sitzt in deutschen Talkshows zum Nahost-Konflikt?"),
  
  
    sidebarLayout(
      sidebarPanel(selectInput("p1_input", h4("Kategorie auswählen"), 
                               choices = list("Profession", "Nationalität",
                                              "Perspektive_Identität", "Geschlecht"), selected = 1)),
      mainPanel(h3(textOutput("t1")),
                plotlyOutput("p1"))
  ),
  
  sidebarLayout(
    sidebarPanel(selectInput("p2_input1", h4("Kategorie auswählen"), 
                             choices = list("Profession", "Nationalität",
                                            "Perspektive_Identität", "Geschlecht"), selected = 1),
                 selectInput("p2_input2", h4("Kategorie auswählen"), 
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
  output$p1 = renderPlotly({
    gg = plot1(variable = input$p1_input, df)
    ggplotly(gg, tooltip = "text")  %>%
      layout(showlegend = FALSE) %>%
      config(displayModeBar = F)# Remove the "plotly" bar
  })
  
  output$t2 = renderText(paste0(input$p2_input1, " vs. ", input$p2_input2,  " der Gäste"))
  output$p2 = renderPlot({
    plot2(variable1 = input$p2_input1, variable2 = input$p2_input2,  df)
  }, bg="transparent")
  
  
  output$t3 = renderText(paste0("Anzahl der Runden pro Woche"))
  output$p3 = renderPlot({
    plot3(start= input$p3_input[1], end = input$p3_input[2],  df)
  }, bg="transparent")
}

# Run the app ----
shinyApp(ui = ui, server = server)
