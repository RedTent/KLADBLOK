library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("controller", "Show", choices = setNames(paste0("panel", 1:3), c("A", "B", "C")))
    ),
    mainPanel(
      tabsetPanel(
        id = "switcher",
        type = "hidden",
        tabPanel("panel1", "Panel 1 content"),
        tabPanel("panel2", "Panel 2 content"),
        tabPanel("panel3", "Panel 3 content")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "switcher", selected = input$controller)
  })
}

shinyApp(ui, server)

