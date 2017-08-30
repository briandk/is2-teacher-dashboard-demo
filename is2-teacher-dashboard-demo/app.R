## app.R ##
library(dplyr)
library(ggplot2)
library(ggvis)
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
    valueBoxOutput("struggling")
  ),
  fluidRow(sliderInput("slider1", label = h3("Amount of Data"), min = 0,
                       max = 500, step = 10, value = 50)),
  fluidRow(
    ggvisOutput("hero_ggvis")
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
        fake_factor = as.factor(sample(1:3, size = number_of_observations, replace = TRUE))) %>%
        mutate(is_struggling = y > 1.0)
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

  hero_ggvis <-
    fake_data %>%
    ggvis(x = ~x,y = ~y, fill = ~ as.factor(is_struggling)) %>%
    layer_points() %>%
    bind_shiny("hero_ggvis")

  struggling_groups <- reactive(
    fake_data() %>%
      filter(is_struggling == TRUE) %>%
      select(is_struggling) %>%
      count() %>%
      pull()
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

  output$struggling <-
    renderValueBox(
      valueBox(
        struggling_groups(),
        "Struggling Groups",
        icon = icon("exclamation-triangle"),
        color = "orange"
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
