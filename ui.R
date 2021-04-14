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
                                 tabPanel("Avance", tabItem(tabName = "dashboard1",
                                                            fluidRow(box(status = "info", plotOutput(outputId = "chart"), height = "2", width = "5")
                                                                     )
                                                            )
                                          ),
                                 tabPanel("General", tabItem(tabName = "dashboard1",
                                                             fluidRow(box(dataTableOutput(outputId = "General"), solidHeader = TRUE, height = "auto", width = "6"),
                                                                      box(dataTableOutput(outputId = "General_socio"), solidHeader = TRUE, height = "auto", width = "6")
                                                               )
                                                             )
                                          )
                                 # tabPanel("Hoy", tabItem(tabName = "dashboard1",
                                 #                            fluidRow(box(dataTableOutput(outputId = "Meta_dia"), solidHeader = TRUE, height = "auto", width = "8")
                                 #                                       )
                                 #                            )
                                 #          )
                                 )
                     )
                     )
)
