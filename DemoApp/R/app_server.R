#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import DT
#' @import dplyr
#' @import plotly
#' @import esquisse
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  filteredData <- reactive({
    data <- mtcars
    if (!"All" %in% input$cylFilter) {
      data <- data[data$cyl %in% input$cylFilter, ]
    }
    if (!"All" %in% input$gearFilter) {
      data <- data[data$gear %in% input$gearFilter, ]
    }
    if (!"All" %in% input$modelFilter) {
      data <- data[row.names(data) %in% input$modelFilter, ]
    }
    data <- data[data$mpg >= input$mpgFilter[1] & data$mpg <= input$mpgFilter[2], ]
    data$`Car Model` <- row.names(data)  # Add 'Car Model' as a column for easier plotting
    data
  })

  output$carPlot <- renderPlotly({
    data <- filteredData()

    # Adjust aesthetic mapping to handle spaces in column names correctly
    aes_input <- list(
      x = if(input$xVar == "Car Model") "`Car Model`" else input$xVar,
      y = if(input$yVar == "Car Model") "`Car Model`" else input$yVar
    )
    if (input$groupVar != "None") {
      aes_input$color <- if(input$groupVar == "Car Model") "`Car Model`" else input$groupVar
      aes_input$fill <- if(input$groupVar == "Car Model") "`Car Model`" else input$groupVar
    }

    # Construct the ggplot object using aes_string and additional aesthetics
    p <- ggplot(data, aes_string(x = aes_input$x, y = aes_input$y, color = aes_input$color, fill = aes_input$fill)) +
      labs(x = input$xVar, y = input$yVar)

    # Apply the appropriate geom based on the graph type
    switch(input$graphType,
           "scatter" = { p <- p + geom_point() },
           "bar" = { p <- p + geom_bar(stat = "identity", position = "dodge") },
           "box" = { p <- p + geom_boxplot() },
           # "box" = { p <- ggplot(data, aes_string(x = aes_input$x, y = aes_input$y, color = aes_input$color, fill = aes_input$fill)) +
           #             geom_boxplot() +
           #             labs(x = input$xVar,
           #                  y = input$yVar)},
           "line" = { p <- p + geom_line(aes(group = input$groupVar)) }
    )

    # Theme adjustments for better readability of x-axis labels
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    p<-ggplotly(p, dynamicTicks = TRUE)
  })

  output$carTable <- DT::renderDT({
    filteredData()
  })


  #Esquisse----
  data_r <- reactiveValues(data = iris, name = "iris")

  observeEvent(input$data, {
    if (input$data == "iris") {
      data_r$data <- iris
      data_r$name <- "iris"
    } else {
      data_r$data <- mtcars
      data_r$name <- "mtcars"
    }
  })

  result<-esquisse_server(
    id="esquisse",
    data=data_r
  )


}
