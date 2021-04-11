library("shiny")
library("shinydashboard")
library("rAmCharts")
library("data.table")
library("dplyr")
library("lubridate")
library("googlesheets4")
library("tibble")
library("ggplot2")

ui = dashboardPage(skin = "blue",
                   dashboardHeader(title = "Avance"),
                   dashboardSidebar(sidebarMenuOutput("menu")),
                   dashboardBody(mainPanel(
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("General", tabItem(tabName = "dashboard1",
                                                             fluidRow(column(12, box(
                                                               dataTableOutput(outputId = "Genera"), solidHeader = TRUE, height = "auto", width = "7")
                                                               )
                                                               )
                                                             )
                                          ),
                                 tabPanel("Semana", tabItem(tabName = "dashboard1",
                                                            fluidRow(box(dataTableOutput(outputId = "Meta"), solidHeader = TRUE, height = "auto", width = "7"),
                                                                     box(status = "info", plotOutput(outputId = "Meta_chart"), height = "auto", width = "5")
                                                                       )
                                                            )
                                          )
                                 )
                                )
                   )
)