library(dplyr)
library(ggplot2)
library(shiny)

load("country-population.rda")

ui <- fluidPage(
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Country",
                choices = c(
                    "Lesotho",
                    "South Africa",
                    "Swaziland"
                )
            )
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        data <- populations %>% filter(name == "South Africa")
        ggplot(data, aes(x = year, y = population)) + geom_point()
    })
}

shinyApp(ui = ui, server = server)
