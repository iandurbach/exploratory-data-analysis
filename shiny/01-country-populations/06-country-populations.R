library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(DT)
library(plotly)

load("country-population.rda")

min_year <- floor(min(populations$year) / 10) * 10
max_year <- ceiling(max(populations$year) / 10) * 10

ui <- fluidPage(
    theme = shinytheme("slate"),
    tags$head(
        tags$style(
            "
@import url('https://fonts.googleapis.com/css?family=Pacifico&display=swap');

h2 {
    font-family: 'Pacifico', cursive;
    font-size: 48px;
    margin-bottom: 25px;
}
ul.nav li a {
    background-color: lightgrey;
}
"
        )
    ),
    titlePanel("Population Explosion"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country", choices = country, selected = "ZAF", multiple = TRUE),
            sliderInput("years", label = "Year range", min = min_year, max = max_year, step = 10, value = c(min_year, max_year)),
            sliderInput("size", "Point size", min = 0, max = 5, value = 1),
            checkboxInput("line", "Plot line"),
            sliderInput("alpha", "Opacity", min = 0, max = 1, step = 0.05, value = 1),
            actionButton("update", "Update", icon = icon("refresh"))
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel(title = "Plot", icon = icon("chart-line"),
                                 plotlyOutput("plot")),
                        tabPanel(title = "Table", icon = icon("table"),
                                 tags$br(),
                                 dataTableOutput("table")),
                        tabPanel(title = "Info", icon = icon("info"),
                                 tags$br(),
                                 uiOutput("info")),
                        tabPanel(title = "Video", icon = icon("youtube"),
                                 tags$br(),
                                 tags$iframe(
                                     src = "https://www.youtube.com/embed/QsBT5EQt348",
                                     width = 560,
                                     height = 315
                                 ),
                                 tags$p(
                                     id = "video-attribution",
                                     tags$a(
                                         "Video", 
                                         href = "https://www.youtube.com/embed/QsBT5EQt348"
                                     ),
                                     "by",
                                     tags$a(
                                         "Kurzgesagt",
                                         href = "https://kurzgesagt.org/",
                                         class = "site"
                                     )
                                 )
                        )
            )
        )
    )
)

server <- function(input, output) {
    data <- reactive({
        populations %>%
            filter(code %in% input$country, between(year, input$years[1], input$years[2]))
    })
    
    observeEvent(input$update, {
        print("Updating opacity!")
    })
    alpha <- eventReactive(input$update, {
        input$alpha
    }, ignoreNULL = FALSE)
    
    output$plot <- renderPlotly({
        p <- ggplot(data(), aes(x = year, y = population / 1000000)) +
            scale_y_log10("Population (million)") +
            theme(legend.title = element_blank())
        if (input$line) p <- p + geom_line(aes(group = code))
        p + geom_point(aes(color = code), size = input$size, alpha = alpha())
    })
    output$table <- renderDataTable({
        data() %>%
            group_by(code) %>%
            arrange(code, desc(year)) %>%
            slice(1) %>%
            ungroup() %>%
            arrange(desc(population))
    })
    output$info <- renderUI({
        tags$p("Data originate from", tags$a(href="https://www.kaggle.com/brajput24/world-population", "Kaggle"), " and contain ", nrow(populations), " records.")
    })
}

shinyApp(ui = ui, server = server)
