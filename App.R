library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(spData)
library(tmap)
theme_set(theme_bw())


data("World")
world = World %>%
  select(name, continent, geometry, economy, income_grp)
data <- read_csv("global_unemployment_data.csv")

data = data %>%
  left_join(world, by = join_by(country_name == name)) %>%
  drop_na() %>%
  pivot_longer(as.character(2014:2024), names_to = "year", values_to = "unemployment") %>%
  mutate(selected = FALSE) %>%
  st_sf()
avg_data = data %>%
  group_by(country_name, year) %>%
  summarize(avg_rate = mean(unemployment))
#data = data %>% pivot_longer(cols = as.character(2014:2024), names_to = "year", values_to = "unemployment")
data$year = as.numeric(data$year)

data_line = data %>%
  group_by(country_name, continent, year, economy) %>%
  summarize(avg = mean(unemployment))


# Define UI
ui <- fluidPage(
  titlePanel("Global Unemployment"),
  br(),
  tabsetPanel(
    tabPanel(
      "World Map",
      fluid = TRUE,
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            sliderInput("years", "Years:", 
                        min = 2014, max = 2024, value = 2014, step = 1, sep = ""),
            width = 3
          ),
          mainPanel(
            leafletOutput("worldmap"),
            p("Click on a country of interest to pull it up"),
            plotOutput("countrymap"),
            width = 9
          )
        )
      )
    ),
    tabPanel(
      "Line Plot",
      fluid = TRUE,
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("region", "Select a region:",
                        choices = c(data_line$continent),
                        selected = data_line[1]),
            width = 3
          ),
          mainPanel(
            strong("NOTE: the green dashed line corresponds to the beginning of COVID"),
            br(),
            br(),
            plotOutput("line_plot", click = "plot_click"),
            width = 9
          )
        )
      )
    ),
    tabPanel(
      "Heat Map",
      fluid = TRUE,
      fluidPage(
        p("Here are the top countries for unemployment by age group"),
        sidebarLayout(
          sidebarPanel(
            selectInput("age", "age range", choices= unique(data %>% pull(age_group))),
            numericInput("number", "number of countries",20, min = 5, max = 100),
            width = 3
          ),
          mainPanel(
            plotOutput("tile_plot"),
            width = 9
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  #selected <- reactiveVal(rep(TRUE, nrow(world)))
  #clicked_year <- reactiveVal(2014)
  
  data2 <- reactive({
    data$country_name <- factor(data$country_name, levels = unique(data %>% 
                                                                     filter(age_group == input$age) %>%
                                                                     group_by(country_name) %>% 
                                                                     mutate(avg = mean(unemployment)) %>% 
                                                                     arrange(desc(avg)) %>% 
                                                                     pull(country_name)))
    data %>% 
      filter(age_group == input$age) %>% arrange(country_name)
  })
  
  world_data <- reactive({
    avg_data %>%
      filter(year == input$years)
  })
  
  # Capture clicked points on the plot
  observeEvent(input$plot_click, {
    clickX <- round(input$plot_click$x)
    updateSliderInput(session, "years", value = round(clickX))
  })
  
  # Render plot based on selected country and number of points
  output$line_plot <- renderPlot({
    selected_data <- subset(data_line, continent == "Asia") %>%
      group_by(year, economy) %>%
      summarize(region_avg = mean(avg))
    
    # Plotting the data
    ggplot(selected_data, aes(x = year, y = region_avg, groups = economy, col = economy)) +
      geom_line() +
      labs(x = "Year", y = "Unemployment Rate",
           title = "Average Unemployment Rate Over Years") +
      geom_vline(xintercept = 2020, color = "green", linetype = 2) +
      geom_vline(xintercept = input$years, color = "red", size = 3)
    })
  
  output$worldmap <- renderLeaflet({
    map = tm_shape(world) +
      tm_polygons(col = "grey") +
      tm_shape(world_data()) +
      tm_polygons(col = "avg_rate")
    
    tmap_leaflet(map)
  })
  
  output$countrymap <- renderPlot({
    chosen = input$worldmap_shape_click$id
    if(!is.null(chosen)) {
      chosen = strsplit(chosen, "\\.")[[1]]
      chosen = paste(chosen[1:length(chosen)-1], collapse = " ")
      if(chosen %in% data$country_name) {
        old = data$selected
        print(old)
        data$selected = replace(data$selected, data$country_name == chosen & data$selected, FALSE)
        print(data$selected)
        if(identical(data$selected, old))
          data$selected = replace(data$selected, data$country_name == chosen & !data$selected, TRUE)
      }
    }
    
    if(identical(data$selected, rep(FALSE, nrow(data)))) {
      data$selected = replace(data$selected, data$country_name == "Afghanistan", TRUE)
    }
    
    data %>%
      filter(selected & year == input$years) %>%
      tm_shape() +
      tm_polygons(col = "unemployment") +
      tm_facets(c("sex", "age_group"))
  })
  
  output$tile_plot <- renderPlot({
    data2() %>% 
      filter(country_name %in% unique(data2() %>% 
                                        group_by(country_name) %>% 
                                        mutate(avg = mean(unemployment)) %>% 
                                        arrange(desc(avg)) %>% 
                                        pull(country_name))[1:input$number]) %>%
      ggplot() +
      geom_tile(aes(x = year, y = country_name, fill = unemployment)) +
      scale_fill_gradientn(colors = c("green", "lightgreen", "yellow", "orange", "red"),
                           guide = "colourbar", limits = c(0, 50), na.value = "red",
                           breaks = c(0, 10, 20, 30, 40, 49), labels = c("0%", "10%", "20%", "30%", "40%", "≥50%")) +
      xlab("Year") + 
      ylab("Country")
  })
}

# Run the application
shinyApp(ui = ui, server = server)