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
  plotOutput("plot_change"),
  plotOutput("plot_age")
)

server <- function(input, output, session) {
  p_change <- reactive(df %>%
                  filter(KOMK == input$municipality) %>%
                  group_by(TID, FKMED) %>%
                  summarise(total = sum(INDHOLD)) %>%
                  mutate(percent = round(total/sum(total) * 100, 1)) %>%
                  filter(FKMED == "Member of National Church") %>%
                  ggplot(., aes(TID, percent)) +
                  geom_line())
  
  age_levels <- unique(df$ALDER)
  
  p_age <- reactive(df %>%
                  filter(TID == max(TID),
                         KOMK == input$municipality) %>%
                  group_by(ALDER, FKMED) %>%
                  summarise(total = sum(INDHOLD)) %>%
                  mutate(percent = round(total/sum(total) * 100, 1)) %>%
                  filter(FKMED == "Member of National Church") %>%
                  mutate(ALDER = factor(ALDER, levels = age_levels)) %>%
                  arrange(ALDER) %>%
                  ggplot(., aes(ALDER, percent)) +
                  geom_col() +
                  coord_flip())
  
  output$plot_change <- renderPlot(p_change(), res = 96, alt = "Alternative text")
  
  output$plot_age <- renderPlot(p_age(), res = 96, alt = "Alternative text")
}

shinyApp(ui, server)
