## app.R ##
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "InquirySpace 2"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Teacher Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)
body <- dashboardBody(
  fluidRow(
    valueBoxOutput("student_count"),
    valueBoxOutput("entropy"),
    valueBox(3,
             "Struggling Groups",
             icon = icon("exclamation-triangle"),
             color = "orange")
  ),
  fluidRow(sliderInput("slider1", label = h3("Amount of Data"), min = 0,
                       max = 500, step = 10, value = 50)),
  fluidRow(
    plotOutput("hero_plot")
  )
)


ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  make_fake_data <- function(number_of_observations) {
    return(
      data.frame(
        x = rnorm(n = number_of_observations),
        y = rnorm(n = number_of_observations),
        fake_factor = as.factor(sample(1:3, size = number_of_observations, replace = TRUE)))
      )
  }

  fake_data <- reactive(make_fake_data(input$slider1))
  mean_entropy <- reactive(
    fake_data() %>%
      summarise(mean_y = mean(y)) %>%
      pull() %>%
      round(digits = 2)
  )

  hero_plot <-
    renderPlot(
      ggplot(
        data = fake_data(),
        aes(x = x, y = y, color = fake_factor)
      ) +
      geom_point() +
      scale_color_brewer(type = "qual", palette = "Set1") +
      theme_minimal()
    )

  output$hero_plot <- hero_plot
  output$student_count <-
    renderValueBox(
      valueBox(
        input$slider1,
        "Current Students",
        icon = icon("user")
      )
    )
  output$entropy <-
    renderValueBox(
      valueBox(mean_entropy(),
               "Average Student Entropy",
               icon = icon("random"),
               color = "green")
    )
}

shinyApp(ui, server)
