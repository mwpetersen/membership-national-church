# A web app showing data on membership of the Danish national church

# packages ----------------------------------------------------------------
library(danstat)
library(shiny)
library(htmltools)
library(tidyverse)

p# import data from Statistic Denmark --------------------------------------
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
            h2("Share af the population that are members of the National Church in ", textOutput("max_year_1", inline = TRUE)),
            
            textOutput("percent_membership")
            ), 
        
        div(class="wide-output",
            
            h2("Change in the share of the population that are members of the National Church"),
            
            plotOutput("plot_change")),
        
        div(class="narrow-output",
            
            h2("Share of men and women among the members of the National Church in ", textOutput("max_year_2", inline = TRUE)),
            
            plotOutput("plot_gender")
            ),
        
        div(class="wide-output",
            
            h2("Share of different age groups that were members of the National Church in ", textOutput("max_year_3", inline = TRUE)),
            
            plotOutput("plot_age")
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
          label = paste0(KØN, ": ", percent, "%")))
  
  output$chosen_municipality <- renderText(input$municipality)
  
  output$percent_membership <- renderText({
    paste0(t_membership(), "%")
    })
  
  output$max_year_1 <- output$max_year_2 <- output$max_year_3 <- renderText({
    t_max_year()
  })
  
  output$plot_change <- renderPlot({
    p_change() %>%
      ggplot(., aes(TID, percent)) +
      geom_line() + 
      theme_minimal()
  }, 
  res = 96, 
  alt = "Alternative text")
  
  output$plot_age <- renderPlot({
    p_age() %>%
      ggplot(., aes(ALDER, percent)) +
      geom_col() +
      coord_flip() +
      theme_minimal()
    }, res = 96, 
    alt = "Alternative text")
  
  output$plot_gender <- renderPlot({
    p_gender() %>%
      ggplot(., aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=KØN)) +
      geom_rect() +
      geom_text(x=1.25, aes(y=labelPosition, label=label), size=4) +
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
      xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
      theme_void() +
      theme(legend.position = "none")
  }, res = 96, alt = "Alternative text")
}

shinyApp(ui, server)
