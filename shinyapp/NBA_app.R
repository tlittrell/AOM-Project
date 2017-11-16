#run the line: source("NBA_app.r", echo = TRUE)
library(shiny)
library(shinydashboard)
library(DT)
library(plyr)

GroupStr <- function(x){
  if (x == 1){
    "Group 1"
  } else if (x == 2){
    "Group 2"
  } else if (x == 3){
    "Group 3"
  } else if (x == 4){
    "Group 4"
  } else if (x == 5){
    "Group 5"
  }
}

ComplSet <- read.csv("TimAllData.csv", header = TRUE)
PlayerNames <- unique(ComplSet$Player)
imNums <- round(runif(length(PlayerNames),0,0.4))
ImDF <- data.frame(PlayerNames, imNums)
ImDF[PlayerNames == "LeBron James",][2] = "Lebron"
ImDF[PlayerNames == "Kobe Bryant",][2] = "Kobe"
coolStats = c('TOV','PF','PTS', 'G', 'MP', 'TRB')

ui <- shinyUI(dashboardPage(
  
  # Application title
  dashboardHeader(title = "NBA Dashboard"),
  dashboardSidebar(disable = TRUE),
  body <- dashboardBody(
    fluidRow(
      box(title = "Player Module",
          fluidRow(
            column(width = 6,
                   box(title = NULL, width = NULL, status = "primary",
                       selectizeInput(inputId = "pname",
                                      label = "Player of Interest",
                                      choices = unique(ComplSet$Player),
                                      multiple = T,
                                      options = list(maxItems = 1, placeholder = 'Select a player'),
                                      selected = "LeBron James"),
                       
                       h4("Player Career Evolution"),
                       
                       DT::dataTableOutput('table'))
            ),
            tags$head(tags$style(
              type="text/css",
              "#currImage img {max-width: 100%; width: 100%; height: 100%}")),
            
            column(width = 6,
                   box(width = NULL, status = "primary",
                       imageOutput("currImage",height = 250)),
                   
                   tags$head( 
                     tags$style(HTML(".fa { font-size: 40px; }"))
                   ),
                   
                   infoBoxOutput("ModeBox", width=12),
                   
                   infoBoxOutput("ModeBoxTeam", width=12)
            )
          ),
          
          fluidRow(
            box(title = "Mean Career Statistics", width = 12, 
                DT::dataTableOutput('table2'))
          )
      ),
      box(title = "Team Module")
    ), 
    fluidRow(
      box(
        title = "Cluster Module", width = 12,
        fluidRow(
          box(
            title = "Scatter Plots", width = 6, background = "light-blue", 
            'contents of this box'),
          box(
            title = "3D Visualization", width = 6, background = "light-blue", 
            'contents of this other box')
        )
      )
    )
  )))

# Define server logic required to plot various variables against mpg
server <- shinyServer(function(input, output, session) {
  
  output$table <- 
    DT::renderDataTable({
      # choose columns to display
      currentPlayer = input$pname
      
      if (is.null(currentPlayer)) {
        currentPlayer = 'LeBron James'
      }
      
      Lebron = ComplSet[ComplSet$Player == currentPlayer,]
      CarEvo = Lebron[,c('Year','Cluster')]
      CarEvo <- rename(CarEvo, c("Cluster" = "Player Type"))
      CarEvo[,2] <- sapply(CarEvo[,2], GroupStr)
      DT::datatable(CarEvo, 
                    rownames= FALSE,
                    options = 
                      list(sDom  = '<"top">rt<"bottom">',
                           scrollY = "300px",
                           pageLength = 25))
    })
  
  # Send a pre-rendered image, and don't delete the image after sending it
  output$currImage <- renderImage({
    currentPlayer = input$pname
    if (is.null(currentPlayer)) {
      currentPlayer = 'LeBron James'
    }
    imID = ImDF[,2][ImDF[,1] == currentPlayer]
    # When input$n is 3, filename is ./images/image3.png
    filename <- normalizePath(file.path('./images',
                                        paste(imID, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", imID),
         width=400)
    
  }, deleteFile = FALSE)
  
  output$ModeBox <- renderInfoBox({
    currentPlayer = input$pname
    
    if (is.null(currentPlayer)) {
      currentPlayer = 'LeBron James'
    }
    
    Lebron = ComplSet[ComplSet$Player == currentPlayer,]
    ModeGroup = 
      GroupStr(as.numeric(names(
        sort(table(Lebron['Cluster']), decreasing = TRUE)))[1])
    
    infoBox(title = "Mode Group", value = ModeGroup,
            icon = icon("dribbble"),
            color = "teal"
    )
  })
  
  output$ModeBoxTeam <- renderInfoBox({
    currentPlayer = input$pname
    
    if (is.null(currentPlayer)) {
      currentPlayer = 'LeBron James'
    }
    
    Lebron = ComplSet[ComplSet$Player == currentPlayer,]
    ModeTeam = 
      names(sort(table(Lebron['Tm']), decreasing = TRUE))[1]
    
    infoBox(title = "Mode Team", value = ModeTeam,
            icon = icon("group"),
            color = "navy"
    )
  })
  
  
  output$table2 <- 
    DT::renderDataTable({
      # choose columns to display
      #Lebron = ComplSet[ComplSet$Player == 'LeBron James',]
      currentPlayer = input$pname
      
      if (is.null(currentPlayer)) {
        currentPlayer = 'LeBron James'
      }
      
      Lebron = ComplSet[ComplSet$Player == currentPlayer,]
      statMeans = sapply(Lebron[,coolStats],mean)
      DT::datatable(data.frame(t(statMeans)), 
                    rownames= FALSE,
                    options = 
                      list(sDom  = '<"top">rt<"bottom">',
                           scrollY = "50px",
                           pageLength = 25))
    })
  
  
})

shinyApp(ui, server)
