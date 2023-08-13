library(shiny)
library(ggplot2)
library(ggfortify)
library(htmltools)

# base plot (basic 20 x 20 grid), should allow negative values (like SLR) 
base_p <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(
    limits = c(-10, 10), breaks = seq(-10, 10, 2), name = "Predictor",
    expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(-10, 10), breaks = seq(-10, 10, 2), name = "Response",
    expand = c(0, 0)) +
  coord_equal() +
  theme_light(base_size = 16)

# ----- ui -----
ui <- fluidPage(
  titlePanel("Clicky Regression"),
  fluidRow(
    column(
      4,
      plotOutput("pointsPlot", click = "points_click", height = 500)
    ),
    column(
      8,
      plotOutput("regDiagnostics", height = 500)
    )
  ),
  fluidRow(
    column(
      4,
      includeMarkdown("instructions.md")
    )
  )
)

# ----- server -----
server <- function(input, output) {
  
  # (record clicks in df of x,y)
  values <- reactiveValues()
  
  values$points <- data.frame(
    x = numeric(),
    y = numeric()
  )
  
  ## show points as they are clicked
  output$pointsPlot <- renderPlot({
    base_p <- base_p + geom_point(data = values$points, aes(x = x, y = y))
    
    ### if there are enough points show the line of best fit
    if (nrow(values$points) > 2) {
      base_p <- base_p + geom_abline(
        slope = best_fit_line()$coefficients[["x"]],
        intercept = best_fit_line()$coefficients[["(Intercept)"]],
        col = "red")
    }
    
    base_p
  })
  
  ## update df when user clicks on plot
  observeEvent(
    input$points_click, {
      add_row <- data.frame(
        x = round(input$points_click$x, 2),
        y = round(input$points_click$y, 2)
      )
      # add row to the data.frame
      values$points <- rbind(values$points, add_row)
    })
  
  best_fit_line <- reactive(
    lm(y ~ x, values$points)
  )
  
  output$regDiagnostics <- renderPlot({
    validate(
      need(nrow(values$points) > 2, 'Add some more points!')
    )
    
    autoplot(best_fit_line()) + theme_bw(base_size = 16)
  })
  
  
}

shinyApp(ui, server)