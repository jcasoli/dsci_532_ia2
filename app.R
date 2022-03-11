library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(tidyverse)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
data <- read_csv("data/processed/survey.csv")

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      dccDropdown(
        id='col-select',
        options = list(list(label = "Q3", value = "Q3"),
                       list(label = "Q11", value = "Q11")),
        value = "Q3"
      )
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(question) {
    question <- sym(question)
    
    map_df <- data %>% filter(Country == "United States") %>%
      select(state, {{question}}) %>%
      group_by(state, {{question}}) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>%
      filter({{question}} == "Yes")
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    fig <- plot_geo(map_df, locationmode = 'USA-states')
    
    fig <- fig %>% add_trace(
      z = ~freq, text = ~state, locations = ~state,
      color = ~freq, colors = 'Purples'
    )
    
    fig <- fig %>%  layout(
      title = 'Mental Health in Tech Map',
      geo = g
    )
    fig
  }
)

app$run_server(host = '0.0.0.0')