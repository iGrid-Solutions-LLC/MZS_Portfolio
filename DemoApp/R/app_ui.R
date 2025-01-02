#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # fluidPage(
    #   h1("DemoApp")
    # )
    dashboardPage(
      dashboardHeader(title = "DemoApp"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Demo", tabName = "graph_table", icon = icon("table"))
        ),
        selectInput("cylFilter", "Number of Cylinders:",
                    choices = c("All", unique(mtcars$cyl)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("modelFilter", "Select Car Model:",
                    choices = c("All", row.names(mtcars)),
                    selected = "All",
                    multiple = TRUE),
        selectInput("gearFilter", "Number of Gears:",
                    choices = c("All", unique(mtcars$gear)),
                    selected = "All",
                    multiple = TRUE),
        sliderInput("mpgFilter", "MPG Range:",
                    min = min(mtcars$mpg),
                    max = max(mtcars$mpg),
                    value = c(min(mtcars$mpg), max(mtcars$mpg)),
                    step = 0.1),
        selectInput("xVar", "Choose X Variable:",
                    choices = c("Car Model", names(mtcars))),
        selectInput("yVar", "Choose Y Variable:",
                    choices = c("Car Model", names(mtcars))),
        selectInput("groupVar", "Choose Grouping Variable:",
                    choices = c("None", "Car Model", names(mtcars)),
                    selected = "None"),
        selectInput("graphType", "Select Graph Type:",
                    choices = c("Scatter Plot" = "scatter", "Bar Chart" = "bar",
                                "Box Plot" = "box", "Line Plot" = "line"))
      ),



      dashboardBody(
        tabItems(
          tabItem(tabName = "graph_table",
            fluidPage(

              tabBox(width = 12,

                tabPanel(
                  "Dashboard",
                  width="100%",

                  fluidPage(
                    column(width=6,
                           plotlyOutput("carPlot", height = "75vh")),
                    column(width=6,
                           DT::DTOutput("carTable"))
                  ) #fluidPage

                ), #tabPanel

                tabPanel(
                  "Graph Builder",
                  width="100%",
                  # h2("New Tab")

                  fluidPage(
#
#
#                     titlePanel("Use esquisse as a Shiny module"),

                    sidebarLayout(
                      sidebarPanel(
                        radioButtons(
                          inputId = "data",
                          label = "Data to use:",
                          choices = c("iris", "mtcars"),
                          inline = TRUE
                        )
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(
                            title = "esquisse",
                            esquisse_ui(
                              id = "esquisse",
                              header = TRUE,
                              container = esquisseContainer(fixed=FALSE),
                              controls = c("labs", "parameters", "appearance", "filters", "code"),
                              insert_code = FALSE
                            )
                          )
                        )
                      )
                    )

                  ) #fluidPage

                ) #tabPanel

              ) #tabBox

            )# fluidPage
          ) #tabItem
        ) #tabItems
      )#dashboardBody
    ) #dashboardPage
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "DemoApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
