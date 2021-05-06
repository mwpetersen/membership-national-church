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
  plotOutput("plot")
)

server <- function(input, output, session) {
  p <- reactive(df %>%
                  filter(KOMK == input$municipality, 
                         FKMED == "Member of National Church") %>%
                  group_by(TID) %>%
                  summarise(total_members = sum(INDHOLD)) %>%
                  ggplot(., aes(TID, total_members)) +
                  geom_line())
  
  output$plot <- renderPlot(p(), res = 96, alt = "Alternative text")
}

shinyApp(ui, server)
