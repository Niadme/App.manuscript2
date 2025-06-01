# App for showcasing data for the sysmtematic review
library(readxl)
library(shiny)
library(tidyverse)


# read in data
data <- read_excel("../datafile.xlsx") %>% 
  select("context", "M_DERS_total") %>%
  mutate(
    M_DERS_total = na_if(M_DERS_total, "NA"),
    M_DERS_total = as.numeric(M_DERS_total)
  )

# define some parameters
population <- c("Forensic psychiatry", "prison population")

# Define UI for application
ui <- fluidPage(
    titlePanel("DERS means"),
    selectInput("pop", "which forensic populaiton would you like to look at", population),
    mainPanel(plotOutput("distPlot"))
    )

# Define server logic
server <- function(input, output) {
    filtered_data <- reactive({
    data %>% 
        filter(
        context == input$pop
        )
      }
    )
    
    output$distPlot <- renderPlot({
        x    <- filtered_data()$M_DERS_total
        hist(x, breaks = 30, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
