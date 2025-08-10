#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(DT)

country_metrics <- read_csv("./Data/countries_metric.csv") %>% 
  mutate(HDI = as.double(`Human Development Index (HDI)`)) %>%
  mutate(Pop = log10(`Population (in millions)`)) %>%  
  mutate(GDP = log10(`Nominal GDP Per capita (in USD)`)) %>%
  mutate(GDP_PPP = log10(`GDP Per capita PPP (in USD)`)) %>% 
  mutate(GINI = as.numeric(GINI)) %>% 
  mutate(
    GINI_bucket = cut(
      GINI,
      breaks = c(-Inf, 30, 35, 40, 45, 50, Inf),
      labels = c("<30", "30-35", "35-40", "40-45", "45-50", "50+"),
      right = FALSE)
  ) %>% 
  filter(country_name != "Niger") %>% 
  drop_na()

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("HDI vs GDP with Population distribution"),
  fluidRow(
    column(6,plotOutput("scatterplot",
                        brush = "myBrush")),
    column(6,plotOutput("histogram",
                        brush = brushOpts("myBrush", direction = "x")))
  ),
  DTOutput("ourData")
)

server <- function(input, output){
  
  emphasisVector = reactiveVal(rep(T, nrow(country_metrics)))
  
  observeEvent(
    input$myBrush,
    emphasisVector(brushedPoints(country_metrics, input$myBrush, allRows = T)$selected_)
  )
  
  output$scatterplot = renderPlot({ggplot(country_metrics, aes(x = GDP, y = HDI, color = GINI_bucket, alpha = emphasisVector()))+
      geom_point()+
      scale_color_viridis_d(option = "turbo")+
      labs(
        x = "GDP",
        color = 'GINI Range',
        caption = "GDP is scaled by Log10 to create Linear Trend"
      )+
      theme_minimal()+
      theme(plot.caption = element_text(size = 10, face = "bold.italic"))+
      guides(alpha = "none")
  })
  
  output$histogram = renderPlot({
    ggplot(country_metrics, aes(Pop)) +
      geom_histogram(fill = "grey")+
      geom_histogram(data = filter(country_metrics, emphasisVector()), fill = "grey10")+
      labs(
        x = "Log10 of Population",
        caption = "Log10 of Population is used to emphisize normal distribution"
      )+
      theme_minimal()+
      theme(plot.caption = element_text(size = 10, face = "bold.italic"))
  })
  
  output$ourData = renderDT(country_metrics %>% 
                              filter(emphasisVector()) %>% 
                              select(-c(GDP,GDP_PPP,GINI_bucket,Pop, HDI)) %>% 
                              rename('Population' = 'Population (in millions)', 'Country' = "country_name") %>% 
                              select(Country, Population, `Human Development Index (HDI)`, GINI, everything()))
}

# Run the application 
shinyApp(ui = ui, server = server)
