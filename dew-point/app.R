library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Cooling Water Condensation Risk Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("temp", "Ambient Air Temperature (°C):", 
                  min = -50, max = 50, value = 25, step = 0.1),
      sliderInput("humidity", "Relative Humidity (%):", 
                  min = 0, max = 100, value = 50, step = 1),
      actionButton("calculate", "Calculate"),
      hr(),
      textOutput("dewPointText"),
      textOutput("minCoolingTempText")
    ),
    
    mainPanel(
      plotOutput("dewPointPlot"),
      hr(),
      tabsetPanel(
        tabPanel("Data", 
                 h4("Dew Point Data"),
                 tableOutput("dewPointData")
        ),
        tabPanel("References",
                 fluidRow(
                   column(12,
                          h4("Technical References"),
                          p("1. Stull, R. B. (2011). Meteorology for Scientists and Engineers: A Technical Companion Book to C. Donald Ahrens' Meteorology Today. Cengage Learning."),
                          p("2. Arya, S. P. (2001). Introduction to Micrometeorology. Academic Press."),
                          p("3. Bolton, D. (1980). The Computation of Equivalent Potential Temperature. Monthly Weather Review, 108(7), 1046–1053."),
                          p("4. Wexler, A. (1976). Vapor Pressure Formulation for Water in Range 0 to 100°C. A Revision. Journal of Research of the National Bureau of Standards - A. Physics and Chemistry, 80A(5), 775–785.")
                   )
                 )
        )
      )
    )
  )
)

# Define server logic required to calculate the dew point and update outputs
server <- function(input, output) {
  
  calcDewPoint <- function(temp, humidity) {
    a <- 17.27
    b <- 237.7
    alpha <- ((a * temp) / (b + temp)) + log(humidity / 100)
    dewPoint <- (b * alpha) / (a - alpha)
    return(dewPoint)
  }
  
  observeEvent(input$calculate, {
    temp <- input$temp
    humidity <- input$humidity
    
    dewPoint <- calcDewPoint(temp, humidity)
    
    output$dewPointText <- renderText({
      paste("Dew Point Temperature: ", round(dewPoint, 2), "°C")
    })
    
    output$minCoolingTempText <- renderText({
      paste("Minimum Cooling Water Temperature to Avoid Condensation: ", round(dewPoint + 1, 2), "°C")
    })
    
    dewPointData <- data.frame(
      Temperature = seq(from = temp - 20, to = temp + 20, by = 1),
      DewPoint = sapply(seq(from = temp - 20, to = temp + 20, by = 1), function(t) calcDewPoint(t, humidity))
    )
    
    output$dewPointData <- renderTable({
      dewPointData
    })
    
    output$dewPointPlot <- renderPlot({
      ggplot(dewPointData, aes(x = Temperature, y = DewPoint)) +
        geom_line(color = "blue") +
        geom_hline(yintercept = dewPoint, linetype = "dashed", color = "red") +
        labs(title = "Dew Point Temperature vs Ambient Temperature",
             x = "Ambient Temperature (°C)",
             y = "Dew Point Temperature (°C)") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
