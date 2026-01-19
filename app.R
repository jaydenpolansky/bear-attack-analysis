# =============================================================================
# Fatal Bear Attacks in North America - Interactive Explorer
# =============================================================================
# Author: Jayden Polansky
# Description: A Shiny app for exploring patterns in fatal bear attacks across
#              North America, examining relationships between timing, bear 
#              species, and victim demographics.
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)

# Load data
bears <- readRDS("data/cleaned_bears.rds")

# =============================================================================
# Helper Functions
# =============================================================================

#' Calculate the mode of a vector
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# =============================================================================
# User Interface
# =============================================================================

ui <- fluidPage(
  
  # App title
  titlePanel("Investigating Variables of Fatal Bear Attacks in North America"),
  
  sidebarLayout(
    # -------------------------------------------------------------------------
    # Sidebar Panel - Controls
    # -------------------------------------------------------------------------
    sidebarPanel(
      # Bear image
      tags$img(
        src = "https://www.thoughtco.com/thmb/5I5yYvEZcTZ361zX35sXRItXIDw=/3867x2578/filters:fill(auto,1)/getty-brown-bear-56afcf215f9b58b7d01d5195.jpg",
        alt = "Brown bear photo",
        width = "190"
      ),
      tags$p(
        tags$a(
          href = "https://www.thoughtco.com/",
          "Photo source: ThoughtCo",
          target = "_blank"
        ),
        style = "font-size: 9px; color: gray;"
      ),
      
      tags$hr(),
      
      # Variable selection
      checkboxGroupInput(
        inputId = "vars",
        label = h4("Variables to Investigate:"),
        choices = list(
          "Month of Year" = "Month",
          "Type of Bear" = "Type.of.bear",
          "Age" = "age",
          "Gender" = "gender"
        ),
        selected = "Month"
      ),
      
      # Color selection
      radioButtons(
        inputId = "colors",
        label = h4("Graph Color:"),
        choices = list(
          "Orange" = "orange",
          "Blue" = "blue",
          "Yellow" = "yellow"
        ),
        selected = "orange"
      ),
      
      # Year range slider
      sliderInput(
        inputId = "yearRange",
        label = h4("Select Range of Years:"),
        min = min(bears$Year),
        max = max(bears$Year),
        value = c(min(bears$Year), max(bears$Year)),
        sep = ""
      ),
      
      tags$hr()
    ),
    
    # -------------------------------------------------------------------------
    # Main Panel - Output
    # -------------------------------------------------------------------------
    mainPanel(
      p(
        strong("Instructions:"),
        "Select variables to explore their relationships in the dataset of fatal ",
        "bear attacks in North America. Choose a graph color and adjust the year ",
        "range to focus on specific time periods."
      ),
      
      # Plot output
      plotOutput("myPlot"),
      
      # Statistics output
      p(strong("Descriptive Statistics:")),
      verbatimTextOutput("statistic")
    )
  )
)

# =============================================================================
# Server Logic
# =============================================================================

server <- function(input, output) {
  
  # ---------------------------------------------------------------------------
  # Reactive Data - Filter by Year Range
  # ---------------------------------------------------------------------------
  filtered_bears <- reactive({
    bears %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
  })
  
  # ---------------------------------------------------------------------------
  # Descriptive Statistics Output
  # ---------------------------------------------------------------------------
  output$statistic <- renderPrint({
    req(input$vars)
    
    data <- filtered_bears()
    num_vars <- length(input$vars)
    
    if (num_vars == 1) {
      # Single variable: mode or five-number summary
      var_selected <- input$vars
      
      if (var_selected == "age") {
        fivenums <- fivenum(data$age)
        paste("Five Number Summary:", paste(fivenums, collapse = ", "))
      } else {
        modevalue <- get_mode(data[[var_selected]])
        paste("Mode:", modevalue)
      }
      
    } else if (num_vars == 2) {
      # Two variables: proportion table
      propTable <- prop.table(table(data[[input$vars[1]]], data[[input$vars[2]]]))
      cat("Proportion Table:\n")
      print(round(propTable, 2))
      
    } else if (num_vars == 3) {
      # Three variables: proportion table
      propTable <- prop.table(table(
        data[[input$vars[1]]],
        data[[input$vars[2]]],
        data[[input$vars[3]]]
      ))
      cat("Proportion Table:\n")
      print(round(propTable, 2))
      
    } else {
      # Four variables: proportion table
      propTable <- prop.table(table(
        data[[input$vars[1]]],
        data[[input$vars[2]]],
        data[[input$vars[3]]],
        data[[input$vars[4]]]
      ))
      cat("Proportion Table:\n")
      print(round(propTable, 2))
    }
  })
  
  # ---------------------------------------------------------------------------
  # Plot Output
  # ---------------------------------------------------------------------------
  output$myPlot <- renderPlot({
    req(input$vars)
    
    data <- filtered_bears()
    year_range <- paste0(input$yearRange[1], "-", input$yearRange[2])
    num_vars <- length(input$vars)
    
    # -------------------------------------------------------------------------
    # Single Variable Plot
    # -------------------------------------------------------------------------
    if (num_vars == 1) {
      
      # Build title based on selected variable
      plot_title <- switch(
        input$vars,
        "Month" = paste("Months of Fatal Bear Attacks in NA from", year_range),
        "Type.of.bear" = paste("Bear Types in Fatal Attacks in NA from", year_range),
        "age" = paste("Victim Ages in Fatal Bear Attacks in NA from", year_range),
        "gender" = paste("Victim Genders in Fatal Bear Attacks in NA from", year_range)
      )
      
      ggplot(data, aes(x = .data[[input$vars]])) +
        geom_bar(fill = input$colors, color = "black") +
        labs(title = plot_title, x = input$vars, y = "Frequency") +
        theme_minimal()
      
    # -------------------------------------------------------------------------
    # Two Variable Plot
    # -------------------------------------------------------------------------
    } else if (num_vars == 2) {
      
      has_age <- "age" %in% input$vars
      
      if (has_age) {
        # Boxplot when age is involved
        ggplot(data, aes(x = .data[[input$vars[1]]], y = .data[[input$vars[2]]])) +
          geom_boxplot(fill = input$colors) +
          labs(
            title = paste("Boxplot of", input$vars[2], "vs", input$vars[1], "from", year_range),
            x = input$vars[1],
            y = input$vars[2]
          ) +
          theme_minimal()
      } else {
        # Grouped bar chart for categorical variables
        ggplot(data, aes_string(x = input$vars[1], fill = input$vars[2])) +
          geom_bar(position = "dodge", color = input$colors) +
          labs(
            title = paste("Bar Graph of", input$vars[2], "vs", input$vars[1], "from", year_range),
            x = input$vars[1],
            y = "Count"
          ) +
          theme_minimal()
      }
      
    # -------------------------------------------------------------------------
    # Three Variable Plot
    # -------------------------------------------------------------------------
    } else if (num_vars == 3) {
      
      if (all(c("age", "Month", "Type.of.bear") %in% input$vars)) {
        # Specific case: age, month, and bear type
        ggplot(data, aes(x = Month, y = age, fill = Type.of.bear)) +
          geom_point(size = 3, alpha = 0.7, color = input$colors, shape = 21) +
          labs(
            title = paste("Age vs Month by Bear Type from", year_range),
            x = "Month",
            y = "Age"
          ) +
          theme_minimal()
        
      } else if ("age" %in% input$vars) {
        # Boxplot with fill when age is involved
        ggplot(data, aes_string(x = input$vars[1], y = input$vars[2], fill = input$vars[3])) +
          geom_boxplot(color = input$colors) +
          labs(
            title = paste("Boxplot of", input$vars[2], "vs", input$vars[1], "by", input$vars[3], "from", year_range),
            x = input$vars[1],
            y = input$vars[2]
          ) +
          theme_minimal()
        
      } else {
        # Faceted stacked bar plot for all categorical
        ggplot(data, aes_string(x = input$vars[1], fill = input$vars[2])) +
          geom_bar(position = "fill", color = input$colors) +
          facet_wrap(~ .data[[input$vars[3]]]) +
          labs(
            title = paste("Stacked Bar Plot of", input$vars[2], "by", input$vars[1], "Faceted by", input$vars[3], "from", year_range),
            x = input$vars[1],
            y = "Proportion"
          ) +
          theme_minimal()
      }
      
    # -------------------------------------------------------------------------
    # Four Variable Plot
    # -------------------------------------------------------------------------
    } else {
      ggplot(data, aes(x = Month, y = age, color = gender, shape = Type.of.bear)) +
        geom_point(size = 3, stroke = 1, fill = input$colors) +
        scale_shape_manual(values = c(21, 22, 23)) +
        labs(
          title = paste("Month vs Age by Gender and Bear Type from", year_range),
          x = "Month",
          y = "Age"
        ) +
        theme_minimal()
    }
  })
}

# =============================================================================
# Run Application
# =============================================================================

shinyApp(ui = ui, server = server)
