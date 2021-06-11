# A web app showing data on membership of the Danish national church

# packages ----------------------------------------------------------------
library(danstat)
library(shiny)
library(htmltools)
library(tidyverse)
library(Cairo)
library(plotly)

# import data from Statistic Denmark --------------------------------------
variables <- list(
  list(
    code = "KOMK",
    values = c(101, 461, 751)),
  list(
    code = "KØN",
    values = NA),
  list(
    code = "ALDER",
    values = NA),
  list(
    code = "FKMED",
    values = NA),
  list(
    code = "Tid",
    values = NA))

df <- danstat::get_data(table_id = "KM6",
                        variables = variables)

# Set global ggplot theme -------------------------------------------------
global_theme <- theme(
  axis.text = element_text(size = 10),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.background = element_rect(fill = "#fbfbfb"),
  panel.background = element_rect(fill = "#fbfbfb"))

# Shiny app ---------------------------------------------------------------
options(shiny.usecairo=TRUE) # set graphics engine to Cairo

municipality_list <- unique(df$KOMK) 

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  div(class="main-content",
  
    h1("Membership of the Danish National Church in Copenhagen, Aarhus and Odense"),
    
    p("This dashboard shows data about membership of the Danish National Church in the three biggest municipalities 
    in Denmark. Below you can choose which municipality data will be shown for. In the
    dashboard you can see the overall share of the population in the chosen municipality that are members of the National 
    church, the change in membership the last ten years, and membership broken down by gender and age group. 
      Data is from Statistics Denmark."),
    
    div(class="select-municipality",
    selectInput("municipality", "Choose municipality:", choices = municipality_list, width = "100%")
    ),
    
    div(class="output-container",
        
        div(class="narrow-output center",
            h2("Share of the population that are members of the National Church in ", textOutput("max_year_1", inline = TRUE)),
            
            textOutput("percent_membership")
            ), 
        
        div(class="wide-output",
            
            h2("Change in the share of the population that are members of the National Church"),
            
            plotlyOutput("plot_change", width = "100%")),
        
        div(class="narrow-output",
            
            h2(class="smaller-padding", "Share of men and women among the members of the National Church in ", textOutput("max_year_2", inline = TRUE)),
            
            plotlyOutput("plot_gender", width = "100%")
            ),
        
        div(class="wide-output",
            
            h2("Share of different age groups that are members of the National Church in ", textOutput("max_year_3", inline = TRUE)),
            
            plotlyOutput("plot_age", width = "100%")
            ),
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df <- reactive(df %>%
    filter(KOMK == input$municipality))
  
  t_membership <- reactive(filtered_df() %>%
    filter(TID == max(TID)) %>%
    group_by(FKMED)%>%
    summarise(total = sum(INDHOLD)) %>%
    mutate(percent = round(total/sum(total) * 100, 1)) %>%
    filter(FKMED == "Member of National Church") %>%
    pull(percent))
  
  t_max_year <- reactive(filtered_df() %>%
    filter(TID == max(TID)) %>% 
    slice_head() %>% 
    pull(TID))
  
  p_change <- reactive(filtered_df() %>%
    group_by(TID, FKMED) %>%
    summarise(total = sum(INDHOLD)) %>%
    mutate(percent = round(total/sum(total) * 100, 1)) %>%
    filter(FKMED == "Member of National Church"))
  
  age_levels <- unique(df$ALDER)
  
  p_age <- reactive(filtered_df() %>%
    filter(TID == max(TID)) %>%
    group_by(ALDER, FKMED) %>%
    summarise(total = sum(INDHOLD)) %>%
    mutate(percent = round(total/sum(total) * 100, 1)) %>%
    filter(FKMED == "Member of National Church") %>%
    mutate(ALDER = factor(ALDER, levels = age_levels)) %>%
    arrange(ALDER))
  
  p_gender <- reactive(filtered_df() %>%
   filter(TID == max(TID),
          FKMED == "Member of National Church") %>%
   group_by(KØN) %>%
   summarise(total = sum(INDHOLD)) %>%
   mutate(percent = round(total/sum(total) * 100, 1), # Compute percentages
          ymax = cumsum(percent), # Compute the cumulative percentages (top of each rectangle)
          ymin = c(0, head(ymax, n=-1)), # Compute the bottom of each rectangle
          labelPosition = (ymax + ymin) / 2,
          label = paste0(KØN, "\n", percent, "%")))
  
  # List with font name - https://stackoverflow.com/questions/57589929/embed-google-font-in-ggplotly-png-download
  f <- list(family = "Roboto")
  a <- list(tickfont = f)
  
  output$chosen_municipality <- renderText(input$municipality)
  
  output$percent_membership <- renderText({
    paste0(t_membership(), "%")
    })
  
  output$max_year_1 <- output$max_year_2 <- output$max_year_3 <- renderText({
    t_max_year()
  })
  
  output$plot_change <- renderPlotly({
    ggplot_change <- p_change() %>%
      ggplot(., aes(TID, 
                    percent,
                    group = 1,
                    text = paste0(
                      "Year: ", TID,"\n",
                      "Percent membership: ", percent, "%")
                    )) +
      geom_line(size = 1, 
                color = "grey35") + 
      scale_y_continuous(limits = c(48, 81),
                         labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = scales::breaks_extended(n = 7)) +
      theme_minimal() +
      global_theme + 
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(size = 0.5)
      )
    
    ggplotly(ggplot_change, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(font=f, xaxis = a, yaxis = a)
  })
  
  output$plot_age <- renderPlotly({
    ggplot_age <- p_age() %>%
      ggplot(., aes(ALDER, 
                    percent,
                    group = 1,
                    text = paste0(
                      "Age group: ", ALDER,
                      "\nPercent membership: ", percent, "%")
                    )) +
      geom_col(width = 0.8) +
      coord_flip() +
      scale_y_continuous(breaks = c(25, 50, 75),
                         labels = function(x) paste0(x, "%")) +
      theme_minimal() +
      global_theme +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())
    
    ggplotly(ggplot_age, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(font=f, xaxis = a, yaxis = a)
    })
  
  output$plot_gender <- renderPlotly({
    m <- list( # Margin and padding
      l = 5,
      r = 5,
      b = 5,
      t = 0,
      pad = 0
    )  
    
    colors <- c('rgb(89,89,89)', 'rgb(166,166,166)')
    
    fig <- plot_ly(p_gender(), labels = ~KØN, values = ~percent, 
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF',
                                           family = "Roboto",
                                           size = 16),
                     hoverinfo = 'text',
                     text = ~paste0(KØN, "\nCount: ", total, "\nPercent of total: ", percent),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', 
                                               width = 1)),
                     showlegend = FALSE) %>%
        add_pie(hole = 0.25) %>%
        config(displayModeBar = FALSE) %>%
        layout(font = f,
               xaxis = list(showgrid = FALSE, 
                            zeroline = FALSE, 
                            showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, 
                            zeroline = FALSE, 
                            showticklabels = FALSE),
               hoverlabel = list(align = "left"),
               margin = m,
               paper_bgcolor = "#fbfbfb")
      
      fig
  })
}

shinyApp(ui, server)
