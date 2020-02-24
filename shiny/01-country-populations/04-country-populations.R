library(dplyr)
library(ggplot2)
library(shiny)

load("country-population.rda")

ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country", choices = country, selected = "ZAF", multiple = TRUE),
            sliderInput("size", "Point size", min = 0, max = 5, value = 1),
            checkboxInput("line", "Plot line")
        ),
        mainPanel(
            plotOutput("plot"),
            tableOutput("table")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        data <- populations %>% filter(code %in% input$country)
        p <- ggplot(data, aes(x = year, y = population / 1000000)) +
            scale_y_log10("Population (million)") +
        if (input$line) p <- p + geom_line(aes(group = code))
        p + geom_point(aes(color = code), size = input$size)
    })
    output$table <- renderTable({
        data <- populations %>%
            filter(code %in% input$country) %>%
            group_by(code) %>%
            arrange(code, desc(year)) %>%
            slice(1)
    })
}

shinyApp(ui = ui, server = server)
