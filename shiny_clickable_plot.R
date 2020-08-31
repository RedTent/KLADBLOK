library(shiny)
library(ggplot2)
# library(rlang)

ui <- basicPage(
  plotOutput("plot", click = "plot_click"),
  tableOutput("data"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  x_val <- reactiveVal(NULL)
  y_val <- reactiveVal(NULL)
  click_data <-  reactiveVal(NULL)
  my_plot <- ggplot(mtcars, aes(wt, mpg)) + 
    geom_point()
  
  
  
  output$plot <- renderPlot({
    my_plot + 
      geom_vline(xintercept = x_val(), linetype = "dashed") + 
      geom_hline(yintercept = y_val(), linetype = "dashed")
  }, res = 96)
  
  observeEvent(input$plot_click, {
    x_val(input$plot_click$x)
    y_val(input$plot_click$y)
    click_data(input$plot_click)
    })

  output$info <- renderPrint({
    # req(input$plot_click)
    c(round(x_val(), 1),
    round(y_val(), 1))
  })
  
  output$data <- renderTable({
    nearPoints(mtcars, click_data(), threshold = 20, addDist = TRUE, maxpoints = 2)
  })
}

shinyApp(ui, server)