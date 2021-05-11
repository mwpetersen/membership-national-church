# A web app showing data on membership of the Danish national church

# packages ----------------------------------------------------------------
library(danstat)
library(shiny)
library(tidyverse)

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


municipality_list <- unique(df$KOMK)

ui <- fluidPage(
  selectInput("municipality", "Choose municipality", choices = municipality_list),
  
  fluidRow(
    column(6, textOutput("percent_membership")),
    column(6, plotOutput("plot_gender"))
  ),
  fluidRow(
    column(6, plotOutput("plot_change")),
    column(6, plotOutput("plot_age"))
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
    arrange(ALDER) %>%
    ggplot(., aes(ALDER, percent)) +
    geom_col() +
    coord_flip())
  
  p_gender <- reactive(filtered_df() %>%
   filter(TID == max(TID),
          FKMED == "Member of National Church") %>%
   group_by(KØN) %>%
   summarise(total = sum(INDHOLD)) %>%
   mutate(percent = round(total/sum(total) * 100, 1), # Compute percentages
          ymax = cumsum(percent), # Compute the cumulative percentages (top of each rectangle)
          ymin = c(0, head(ymax, n=-1)), # Compute the bottom of each rectangle
          labelPosition = (ymax + ymin) / 2,
          label = paste0(KØN, ": ", percent, "%")) %>%
   ggplot(., aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=KØN)) +
   geom_rect() +
   geom_text(x=1.25, aes(y=labelPosition, label=label), size=4) +
   scale_fill_brewer(palette=3) +
   scale_color_brewer(palette=3) +
   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
   xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
   theme_void() +
   theme(legend.position = "none"))

  output$percent_membership <- renderText({
    paste0("Share af the population that are members of the National Church: ", t_membership(), "%")
    })
  
  output$plot_change <- renderPlot({
    p_change() %>%
      ggplot(., aes(TID, percent)) +
      geom_line()
  }, 
  res = 96, 
  alt = "Alternative text")
  
  output$plot_age <- renderPlot(p_age(), res = 96, alt = "Alternative text")
  
  output$plot_gender <- renderPlot(p_gender(), res = 96, alt = "Alternative text")
}

shinyApp(ui, server)
