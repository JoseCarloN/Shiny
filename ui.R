library("shiny")
library("shinydashboard")
library("data.table")
library("dplyr")
library("lubridate")
library("googlesheets4")
library("tibble")
library("ggplot2")
library("DT")

dir.create('~/.fonts')
file.copy(c("www/Barlow-Black.ttf", "www/Barlow-SemiBold.ttf"), "~/.fonts")
system('fc-cache -f ~/.fonts')

ui = dashboardPage(skin = "blue",
                   dashboardHeader(title = "Avance"),
                   dashboardSidebar(sidebarMenuOutput("menu")),
                   dashboardBody(mainPanel(
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("General", tabItem(tabName = "dashboard1",
                                                             fluidRow(box(status = "info", plotOutput(outputId = "General_chart"), height = "auto", width = "5"),
                                                                      box(dataTableOutput(outputId = "General"), solidHeader = TRUE, height = "auto", width = "7")
                                                               )
                                                             )
                                          ),
                                 tabPanel("Hoy", tabItem(tabName = "dashboard1",
                                                            fluidRow(box(dataTableOutput(outputId = "Meta_dia"), solidHeader = TRUE, height = "auto", width = "7")
                                                                       )
                                                            )
                                          )
                                 )
                                )
                   )
)