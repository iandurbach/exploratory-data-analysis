library(dplyr)
library(ggplot2)
library(shiny)

load("country-population.rda")

ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country", choices = country, selected = "ZAF"),
            sliderInput("size", "Point size", min = 0, max = 5, value = 1),
            checkboxInput("line", "Plot line")
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        data <- populations %>% filter(code == input$country)
        p <- ggplot(data, aes(x = year, y = population))
        if (input$line) p <- p + geom_line()
        p + geom_point(size = input$size)
    })
}

shinyApp(ui = ui, server = server)
