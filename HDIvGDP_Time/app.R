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
library(tsibble)
library(shinyWidgets)
library(plotly)
#Pre-Processsing for App
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

GDP_Time <- read_csv("./Data/GDP.csv", col_names = TRUE) 
GDP_Time <- GDP_Time %>% pivot_wider(names_from = Series,
                                     values_from = Value) %>% 
  select(-c(Footnotes, Source))

GDP_Time <- GDP_Time %>% 
  group_by(`Region/Country/Area`, Year) %>% 
  summarise(across(everything(), ~first(na.omit(.))), .groups = "drop")

HDI_Time <- read_csv("./Data/HDI.csv") %>% 
  mutate(across(c(`1990`,`2000`,`2010`,`2015`,`2020`,`2021`,`2022`,`2023`), as.numeric)) %>% 
  pivot_longer(cols = c(`1990`,`2000`,`2010`,`2015`,`2020`,`2021`,`2022`,`2023`), names_to = "Year", values_to = "HDI") %>%
  mutate(Year = as.numeric(Year)) %>% 
  drop_na()

countries <- country_metrics %>% distinct(country_name)

GDP_ts <- GDP_Time %>%
  filter(`Region/Country/Area` %in% countries$country_name) %>%
  left_join(country_metrics %>% select(country_name, GINI), by = c(`Region/Country/Area` = "country_name")) %>% 
  as_tsibble(index = Year, key = `Region/Country/Area`)

GDP_HDI_ts <- GDP_ts %>% inner_join(HDI_Time, by = c("Region/Country/Area" = "Country", 'Year'))
# Define UI for application that draws a histogram
yrs <- unique(GDP_HDI_ts$Year)

ui <- fluidPage(
  titlePanel("HDI vs GDP Trends by Country"),
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      sliderTextInput(
        "year",
        "Select Year:",
        choices = yrs,
        selected = yrs[1],
        animate = animationOptions(interval = 800, loop = TRUE)
        
      ),
      selectInput(
        "country",
        "Select Countries to Highlight",
        choices = c(countries$country_name),
        selected = c(sample(countries$country_name, 8)),
        multiple = T
      ),
      helpText("Use the slider to animate through years or pause to explore the data at a given year. Select countries to see their trend line over time.")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("scatter",
                   height = "600px"),
      wellPanel(
        style = "padding: 10px; margin-top: 10px;",
        tags$em("GDP (in millions USD) scaled by Log10")
      )
    )
  )
)

server <- function(input, output){
  output$scatter <- renderPlotly({
    df_yrs <- GDP_HDI_ts %>% filter(Year == input$year)
    
    highlighted_years <- GDP_HDI_ts %>% 
      filter(`Region/Country/Area` %in% input$country, Year <= input$year) %>% 
      arrange(`Region/Country/Area`, Year)
    
    p <- ggplot(df_yrs, 
                aes(y = HDI, 
                    x = log10(`GDP in current prices (millions of US dollars)`),
                    text = paste(
                      "Country:", `Region/Country/Area`,
                      "<br>Year", Year,
                      "<br>HDI:", round(HDI, 3),
                      "<br>GDP per capita:", scales::comma(`GDP per capita (US dollars)`),
                      "<br>GINI:", GINI
                    )
                )
    ) +
      geom_point(color = "grey50", alpha = 0.75) +
      geom_point(data = df_yrs %>% filter(`Region/Country/Area` %in% input$country), 
                 aes(colour =`Region/Country/Area`),
                 size = 2.5)+
      geom_path(data = highlighted_years, 
                aes(y = HDI, 
                    x = log10(`GDP in current prices (millions of US dollars)`), 
                    colour =`Region/Country/Area`,
                    group = `Region/Country/Area`
                ),
                linewidth = 1.25)+
      xlim(1.5, 8) +
      ylim(0, 1) +
      scale_color_viridis_d(option = "turbo")+
      labs(
        subtitle = "GPD Displayed in log scale to emphasize linear trend",
        colour = "Country"
      ) +
      xlab("GDP")+
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
