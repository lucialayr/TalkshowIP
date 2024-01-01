library(shiny)
library(shinythemes)
library(tidyverse)
library(scico)
library(lubridate)
library(plotly)


df = read_csv("data/data.csv")

colorscheme = "lapaz"

cutoff = function(df, variable, thresh = 1) {
  df0 = df %>%
    select(!!rlang::sym(variable), Name, Datum) %>%
    unique() %>%
    group_by(!!rlang::sym(variable)) %>%
    count() %>%
    mutate(indicator = case_when(!!rlang::sym(variable) %in% c("deutsch", "israelisch", 
                                                               "jüdisch", "palästinensisch") ~ !!rlang::sym(variable),
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

plot1 = function(variable, df=df, thresh = 2) {
  df0 = cutoff(df, variable, thresh)
  
  label_andere = df0 %>%
    filter(n <= thresh) %>%
    mutate(label = paste0(!!rlang::sym(variable), ": ", n, " Auftritte<br>")) %>%
    ungroup() 
  
  label = paste(label_andere$label, collapse = " ")
  
  df1 = full_join(df, df0) %>%
    select(!!rlang::sym(variable), Name, Datum, indicator) %>%
    unique() %>%
    group_by(indicator) %>%
    count() %>%
    mutate(text = if_else(indicator == "andere", as.character(label), paste0(as.character(indicator), ": ", n, " Auftritte")))
  
  p1 = ggplot() + theme_void() +
      geom_bar(data = df1, alpha = 1, color = "black", stat = "identity", width = .5,
               aes(x = indicator, y =n,  fill = indicator, text = text)) +
      scico::scale_fill_scico_d(palette = colorscheme, direction = 1, end = .85, begin = .3) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(name = "") + 
    add_plot_layout(fontsize = 15) +
      theme(axis.text.x = element_text(size = 15, angle = 90),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.line.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey80"),
            legend.position = "None"
      )
  
  return(p1)
}

plot2 = function(variable1, variable2, df, thresh = 2) {
  
  df01 = cutoff(df, variable1, thresh = thresh)
  
  andere1 = df01[df01$indicator == "andere", ][[variable1]]
  
  df02 = cutoff(df, variable2, thresh = thresh)
  
  andere2 = df02[df02$indicator == "andere", ][[variable2]]
  
  df2 = df %>%
    mutate(!!rlang::sym(variable1) := if_else(!!rlang::sym(variable1) %in% andere1, 
                                            "andere", !!rlang::sym(variable1)),
           !!rlang::sym(variable2) := if_else(!!rlang::sym(variable2) %in% andere2, 
                                              "andere", !!rlang::sym(variable2))) %>%
    group_by(across(c(-Datum, -Titel, -Sendung))) %>%
    count() %>%
    mutate(n = as.integer(n)) %>%
    ungroup %>%
    select(n, Name, !!rlang::sym(variable1), !!rlang::sym(variable2)) %>%
    distinct() 
  
  grid = df %>%
    select(!!rlang::sym(variable1), !!rlang::sym(variable2))
  
  v1 = unique(df01[,3]) %>% pull()
  v2 = unique(df02[,3]) %>% pull()
  
  grid = expand.grid(v1, v2) 
  
  (p2 = ggplot() + theme_void() + 
    geom_tile(data = grid, aes(x = Var1, y = Var2, fill = NA, colour = 'grey80'), alpha = .3, linewidth = .05) +
    geom_line(data = df2, position = position_jitter(width = .4, height = .4, seed = 123), linewidth = .3, alpha = .9, color = "grey0",
               aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2), group = Name)) +
    geom_point(data = df2, shape = 21,  position = position_jitter(width = .4, height = .4, seed = 123),  alpha = .9,
                 aes(x = !!rlang::sym(variable1), y = !!rlang::sym(variable2),  fill = !!rlang::sym(variable1), size = n, text = Name)) +
    scale_size_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0,max(df2$n))) +
    scico::scale_fill_scico_d(palette = colorscheme, direction = 1, end = .85, begin = .3) +
    scale_x_discrete(expand = c(0,0), name = variable1) +
    scale_y_discrete(expand = c(0,0), name = variable2) +
    add_plot_layout(fontsize = 15) +
    theme(axis.text.x = element_text(size = 15, angle = 90),
          axis.text.y = element_text(size = 15),
           legend.position = "None"
     ))
  
  return(p2)
}

plot3 = function(start, end, df, variable, thresh = 2) {
  
  print(start)
  
  if (variable %in% c("Nationalität", "Perspektive_Identität")) thresh = thresh else thresh = 0
  # area plot needs a second dataframe, where everz guest is 1/no guests in that specific show so that y axis is the same
  # dots should give info of shows and when and which guests.
  
  df_date = df %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%y")) %>%
    filter(Datum >= as.Date(start), Datum <= as.Date(end)) %>%
    mutate(Week = lubridate::week(Datum),
           Sunday = Datum - lubridate::wday(Datum) + 1) %>%
    group_by(Week, Sunday, Datum) %>%
    count() %>%
    group_by(Week, Sunday) %>%
    count()
  
  df01 = df %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%y")) %>%
    filter(Datum >= as.Date(start), Datum <= as.Date(end)) %>%
    mutate(Week = lubridate::week(Datum),
           Sunday = Datum - lubridate::wday(Datum) + 1) %>%
    group_by(!!rlang::sym(variable), Week) %>%
    count() %>%
    mutate(indicator = case_when(!!rlang::sym(variable) %in% c("deutsch", "israelisch", 
                                                               "jüdisch", "palästinensisch") ~ !!rlang::sym(variable),
                                 n > thresh ~ !!rlang::sym(variable),
                                 TRUE ~ "andere")) 
  
  df_area = df %>%
    mutate(Datum = as.Date(Datum, format = "%d.%m.%y")) %>%
    filter(Datum >= as.Date(start), Datum <= as.Date(end)) %>%
    mutate(Week = lubridate::week(Datum),
           Sunday = Datum - lubridate::wday(Datum) + 1) %>%
    select(Week, Datum, Sunday, Name, !!rlang::sym(variable)) %>%
    unique() %>%
    group_by(Week, Sunday, !!rlang::sym(variable)) %>%
    count(name = "variable_per_week") %>%
    mutate(indicator = case_when(!!rlang::sym(variable) %in% c("deutsch", "israelisch", 
                                                               "jüdisch", "palästinensisch",
                                                               unique(df$Profession)) ~ !!rlang::sym(variable),
                                 variable_per_week > thresh ~ !!rlang::sym(variable),
                                 TRUE ~ "andere")) %>%
    ungroup() %>%
    group_by(Week, Sunday, indicator) %>%
    mutate(variable_per_week = sum(variable_per_week)) %>%
    select(Week, Sunday, variable_per_week, indicator) %>%
    unique() %>%
    group_by(Week) %>%
    mutate(share = variable_per_week/sum(variable_per_week)) %>%
    left_join(df_date) %>%
    mutate(share = share*n) %>%
    ungroup()
  
  all_comb = df_area %>%
    expand(Sunday, indicator) %>%
    left_join(df_area) %>%
    mutate(share = replace_na(share, 0))
    
  ggplot() + theme_void() + 
    geom_area(data = all_comb, aes(x = Sunday + 1, y = share, fill = indicator), color = "black", alpha = 1) +
    geom_line(data = df_date, aes(x = Sunday + 1, y = n)) +
    #geom_point(data = df_date, fill = "grey50", color = "black", shape = 21, position = "identity",
    #           aes(x = Sunday + 1, y = n, size = n)) +
    add_plot_layout(fontsize = 15) +
    scale_x_date(name = "Wochenbeginn (Sonntag)") +
    scale_y_continuous(limits = c(0, max(df_date$n)), expand = c(0,.5)) +
    scale_size_continuous(limits = c(1, max(df_date$n)), range = c(1, max(df_date$n)*2)) +
    scico::scale_fill_scico_d(palette = colorscheme, direction = 1, name = variable, end = .85, begin = .3)  + 
    theme(axis.text.x = element_text(size = 15, angle = 90),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.y = element_line(color = "grey80")
    )
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
                  
    titlePanel( "Wer sitzt in deutschen Talkshows zu Israel und Palästina?", 
                p("p creates a paragraph of text.")
    ),
    p("Für diese Analyse wurden alle Sendungen der Format 'Maybrit Illner', 'Anne Will' und 'Markus Lanz' 
                    seit dem 7. Oktober berücksichtigt, die sich mit Israel und Palästina beschäftigten.
                    Die Charakterisierung wurde von mir vorgenommen. Wenn Sie einen Fehler entdecken, 
                    oder sonstiges Feedback haben, schreiben Sie mir bitte an lucialayr@gmail.com", 
      style = "text-align: justify; font-style: italic;"),
  
  
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
                             choices = list("Nationalität", "Profession", 
                                            "Perspektive_Identität", "Geschlecht"), selected = 2)),
    mainPanel(h3(textOutput("t2")),
              plotlyOutput("p2"))
  ),
  
  sidebarLayout(
    sidebarPanel(dateRangeInput("p3_input1", h4("Zeitraum auswählen"), format = "yyyy-mm-dd", start  = "2023-10-07", min =  "2023-10-07"),
                 selectInput("p3_input2", h4("Kategorie auswählen"), 
                             choices = list("Nationalität", "Profession", 
                                            "Perspektive_Identität", "Geschlecht"), selected = 2)),
    mainPanel(h3(textOutput("t3")), 
              plotlyOutput("p3"))
  ),
  
 
  
  
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
  output$p2 = renderPlotly({
    gg = plot2(variable1 = input$p2_input1, variable2 = input$p2_input2,  df)
    ggplotly(gg, tooltip = "text")  %>%
      layout(showlegend = FALSE, aspectratio = list(x = 2, y = 2),
             xaxis = list(dtick = 2, rangemode = "tozero"), yaxis = list(dtick = 2, rangemode = "tozero")) %>%
      config(displayModeBar = F)# Remove the "plotly" bar
  })
  
  
  output$t3 = renderText(paste0("Anzahl der Runden pro Woche"))
  output$p3 = renderPlotly({
    gg = plot3(start= input$p3_input1[1], end = input$p3_input1[2],  df, input$p3_input2)
    ggplotly(gg, tooltip = "text")  %>%
      layout(aspectratio = list(x = 2, y = 2),
             xaxis = list(dtick = 2, rangemode = "tozero"), yaxis = list(dtick = 2, rangemode = "tozero")) %>%
      config(displayModeBar = F)# Remove the "plotly" bar
    
    
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
