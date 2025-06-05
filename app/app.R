# app.R

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("This app let´s you see which measures of emotion regulation are the most common"),
  sidebarLayout(
    sidebarPanel(
      helpText("The pie chart to the right provides an overview of which measures of emotion reuglation that figure most commonly within forensic settings. 
               For simplicity various versions of the same measure (e.g. DERS-36 and DERS-16) are collapsed to one category (i.e. DERS). The same is true 
               for all other measures displayed in this chart. For more informaiton please visit the original article"),
      hr(),
      helpText("In this section you can find estimates of reliability filtered for all measures. Please filter according to what measure you would like to look at"),
      selectInput(
        inputId = "relcol",
        label   = "Select measure:",
        choices = NULL, 
        selected = NULL
      )
    ),
    mainPanel(
      plotOutput("piePlot"),
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output, session) {

  df <- read_excel("../datafile.xlsx")

  df_grouped <- df %>%
    mutate(
      measure_group = case_when(
        grepl("^DERS", primary_measure, ignore.case = TRUE)        ~ "DERS",
        grepl("^TMMS", primary_measure, ignore.case = TRUE)        ~ "TMMS",
        grepl("^CERQ", primary_measure, ignore.case = TRUE)        ~ "CERQ",
        grepl("^ERQ", primary_measure, ignore.case = TRUE)         ~ "ERQ",
        primary_measure %in% c("fMRI", "HR", "HR, SC")             ~ "biophysiological",
        TRUE                                                       ~ primary_measure
      )
    ) %>%
    count(measure_group) %>%
    mutate(
      pct   = n / sum(n) * 100,
      label = paste0(measure_group, "\n", sprintf("%.1f%%", pct))
    )
  
  output$piePlot <- renderPlot({
    ggplot(df_grouped, aes(x = "", y = n, fill = measure_group)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = label), position = position_stack(vjust = 1), size = 4) +
      theme_void() +
      guides(fill = guide_legend(title = "Measure"))
  })
  
  # 2) Determine which columns start with "alpha_"
  rel_cols <- df %>% select(starts_with("alpha_")) %>% names()
  
  # Populate the "relcol" dropdown with those column names:
  observe({
    updateSelectInput(
      session,
      "relcol",
      choices  = rel_cols,
      selected = rel_cols[1]
    )
  })
  
  # 3) Single‐column scatterplot: plot all non‐missing values for input$relcol,
  #    jittered on the x‐axis, with y‐axis fixed from 0 to 1
  output$scatterPlot <- renderPlot({
    req(input$relcol)
    
    df_scatter <- df %>%
      mutate(
        reliability = as.numeric(.data[[input$relcol]])
      ) %>%
      filter(!is.na(reliability)) %>%
      mutate(measure_name = input$relcol)
    
    ggplot(df_scatter, aes(x = measure_name, y = reliability)) +
      geom_point(
        size     = 2,
        alpha    = 0.6,
        position = position_jitter(width = 0.1)
      ) +
      labs(
        x     = "",
        y     = "Cronbach alpha",
        title = paste0("Reliability values for ", input$relcol)
      ) +
      scale_y_continuous(limits = c(0, 1)) +
      geom_hline(yintercept = 0.7, color = "black", linetype = "dashed") +
      theme_minimal()
  })
}

shinyApp(ui, server)