library(shiny)
library(bslib)

ui <- page_fillable(
  title = "Confidence Interval for Two Population Proportions",
  theme = bs_theme(primary = "#A90533"),
  
  # App title
  div(
    style = "text-align: left; margin-bottom: 20px; background-color: #A90533; padding: 20px; border-radius: 5px;",
    h1("Confidence Interval for Two Population Proportions", 
       style = "color: white; font-weight: bold; margin: 0;")
  ),
  
  card(
    card_header(
      "Input Parameters",
      style = "background-color: #A90533; color: white; font-weight: bold;"
    ),
    
    tabsetPanel(
      id = "input_type",
      
      tabPanel("Count Input",
        br(),
        layout_columns(
          col_widths = c(4, 4, 4),
          
          div(
            h4("Sample 1"),
            numericInput("count1", 
                         "Number of Successes:", 
                         value = 50, 
                         min = 0, 
                         step = 1),
            numericInput("total1", 
                         "Sample Size:", 
                         value = 100, 
                         min = 1, 
                         step = 1)
          ),
          
          div(
            h4("Sample 2"),
            numericInput("count2", 
                         "Number of Successes:", 
                         value = 30, 
                         min = 0, 
                         step = 1),
            numericInput("total2", 
                         "Sample Size:", 
                         value = 80, 
                         min = 1, 
                         step = 1)
          ),
          
          div(
            h4("Confidence Level"),
            numericInput("confidence_level_count", 
                         "Confidence Level (%):", 
                         value = 95, 
                         min = 1, 
                         max = 99.9, 
                         step = 0.1)
          )
        )
      ),
      
      tabPanel("Proportion Input",
        br(),
        layout_columns(
          col_widths = c(4, 4, 4),
          
          div(
            h4("Sample 1"),
            numericInput("prop1", 
                         "Sample Proportion:", 
                         value = 0.5, 
                         min = 0, 
                         max = 1, 
                         step = 0.001),
            numericInput("total1_prop", 
                         "Sample Size:", 
                         value = 100, 
                         min = 1, 
                         step = 1)
          ),
          
          div(
            h4("Sample 2"),
            numericInput("prop2", 
                         "Sample Proportion:", 
                         value = 0.375, 
                         min = 0, 
                         max = 1, 
                         step = 0.001),
            numericInput("total2_prop", 
                         "Sample Size:", 
                         value = 80, 
                         min = 1, 
                         step = 1)
          ),
          
          div(
            h4("Confidence Level"),
            numericInput("confidence_level_prop", 
                         "Confidence Level (%):", 
                         value = 95, 
                         min = 1, 
                         max = 99.9, 
                         step = 0.1)
          )
        )
      )
    )
  ),
  
  card(
    card_header(
      "Results",
      style = "background-color: #A90533; color: white; font-weight: bold;"
    ),
    tableOutput("results_table")
  )
)

server <- function(input, output, session) {
  
  # Validate inputs for count tab
  observeEvent(c(input$count1, input$total1), {
    if (!is.null(input$count1) && !is.null(input$total1)) {
      if (input$count1 > input$total1) {
        updateNumericInput(session, "count1", value = input$total1)
      }
    }
  })
  
  observeEvent(c(input$count2, input$total2), {
    if (!is.null(input$count2) && !is.null(input$total2)) {
      if (input$count2 > input$total2) {
        updateNumericInput(session, "count2", value = input$total2)
      }
    }
  })
  
  # Sync confidence level between tabs
  observeEvent(input$confidence_level_count, {
    updateNumericInput(session, "confidence_level_prop", value = input$confidence_level_count)
  })
  
  observeEvent(input$confidence_level_prop, {
    updateNumericInput(session, "confidence_level_count", value = input$confidence_level_prop)
  })
  
  # Calculate results
  results <- reactive({
    # Get confidence level based on active tab
    confidence_level <- if (input$input_type == "Count Input") {
      req(input$confidence_level_count)
      input$confidence_level_count
    } else {
      req(input$confidence_level_prop)
      input$confidence_level_prop
    }
    
    # Get values based on active tab
    if (input$input_type == "Count Input") {
      req(input$count1, input$total1, input$count2, input$total2)
      
      # Basic validation
      if (input$total1 <= 0 || input$total2 <= 0) return(NULL)
      if (input$count1 < 0 || input$count2 < 0) return(NULL)
      if (input$count1 > input$total1 || input$count2 > input$total2) return(NULL)
      
      # Calculate proportions
      p1 <- input$count1 / input$total1
      p2 <- input$count2 / input$total2
      count1 <- input$count1
      count2 <- input$count2
      total1 <- input$total1
      total2 <- input$total2
      
    } else {
      req(input$prop1, input$total1_prop, input$prop2, input$total2_prop)
      
      # Basic validation
      if (input$total1_prop <= 0 || input$total2_prop <= 0) return(NULL)
      if (input$prop1 < 0 || input$prop2 < 0 || input$prop1 > 1 || input$prop2 > 1) return(NULL)
      
      # Use proportions directly
      p1 <- input$prop1
      p2 <- input$prop2
      count1 <- round(input$prop1 * input$total1_prop)
      count2 <- round(input$prop2 * input$total2_prop)
      total1 <- input$total1_prop
      total2 <- input$total2_prop
    }
    
    # Sample difference (p1 - p2)
    sample_diff <- p1 - p2
    
    # Standard error for difference of proportions
    se <- sqrt((p1 * (1 - p1) / total1) + (p2 * (1 - p2) / total2))
    
    # Critical value (z-score)
    alpha <- (100 - confidence_level) / 100
    z_crit <- qnorm(1 - alpha/2)
    
    # Confidence interval
    margin_error <- z_crit * se
    lower_limit <- sample_diff - margin_error
    upper_limit <- sample_diff + margin_error
    
    # Return results as a wide format data frame with z* included
    data.frame(
      `Count 1` = count1,
      `Total 1` = total1,
      `Count 2` = count2,
      `Total 2` = total2,
      `Sample Difference` = round(sample_diff, 6),
      `Standard Error` = round(se, 6),
      `z*` = round(z_crit, 4),
      `Lower Limit` = round(lower_limit, 6),
      `Upper Limit` = round(upper_limit, 6),
      check.names = FALSE
    )
  })
  
  output$results_table <- renderTable({
    results()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
