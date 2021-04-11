library("shiny")
library("shinydashboard")
library("rAmCharts")
library("data.table")
library("dplyr")
library("lubridate")
library("googlesheets4")
library("tibble")

ui = dashboardPage(skin = "blue",
                   dashboardHeader(title = "Tablero"),
                   dashboardSidebar(
                     sidebarMenuOutput("menu")
                   ),
                   dashboardBody(
                     
                     tabItems(
                       tabItem(tabName = "dashboard1",
                               fluidRow(
                                 column(12, box(
                                   dataTableOutput(outputId = "Meta"), solidHeader = TRUE, height = "auto", width = "auto")
                                 )
                               )
                       )
                     )
                   )
)