#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
setwd("~/Dropbox (Personal)/MIT/Fall 2017/Analytics of Operations Management/Project/AOM-Project")
data = read.csv("clustered_data.csv", header = TRUE) %>%
  mutate(PlayerYear = paste(Player, Year)) 

# Define UI for application that draws a histogram
ui <- shinyUI(dashboardPage(
  dashboardHeader(title = "NBA Dashboard"),
  dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      box(title = "Team module",
          fluidRow(
            column(width = 5,
              box(title = "Team roster", width = NULL, status = "primary",
                selectizeInput(
                  inputId = "pnames",
                  label = NULL,
                  choices = data$PlayerYear,
                  multiple = T,
                  options = list(maxItems = 5, placeholder = 'Select a player')
                )
              )
            ),
            column(width = 7,
              box(title = "Summary stats (per 36 min)", width = NULL, status = "primary",
                  DT::dataTableOutput('team_summary_tbl')
              )
            )
          )
         )
    )
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$team_summary_tbl <- 
    DT::renderDataTable({
      temp = data %>%
        filter(PlayerYear %in% input$pnames) %>%
        summarise("Points" = sum(PTS),
                  "Threes" = sum(X3P),
                  "Twos" = sum(X2P),
                  "Off. Rebounds" = sum(ORB),
                  "Def. Rebounds" = sum(DRB),
                  "Assists" = sum(AST),
                  "Steals" = sum(STL),
                  "Blocks" = sum(BLK),
                  "Turnovers" = sum(TOV),
                  "Personal Fouls" = sum(PF),
                  "Average Pos" = mean(PosNum),
                  "Average Cluster" = mean(cluster)) %>% 
        t() %>%
        data.frame() %>%
        `colnames<-`("Team total")
   },
   options = list(pageLength = 12, searching = FALSE, lengthChange = FALSE, paging = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)


