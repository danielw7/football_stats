#----------------------------------------------------------------------------------------------#
#                                                                                              #
#                         create a football betting app - the app                              #
#                                                                                              #
#----------------------------------------------------------------------------------------------#

##  libraries  ##
library(shiny)
library(shinythemes)
library(DT)

#------------------------------------------------------------------------#
#                               load data                                #
#------------------------------------------------------------------------#

source("./data_work.R")

#------------------------------------------------------------------------#
#                               Shiny app                                #
#------------------------------------------------------------------------#

#--------------------------------#
#            UI                  #
#--------------------------------#

ui <- fluidPage(
  titlePanel("Football Statistics"),
  
  theme = shinytheme("flatly"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      (""),
      br(),
      selectInput("league", "Choose League", choices = data_full$Div, selected = c(" England - Premier League")),
      
      uiOutput("hometeam"),
      
      uiOutput("awayteam")
      
  ),
    
    mainPanel(tabsetPanel(
      
      tabPanel("Goals",
               br(),
               (DT::dataTableOutput("table_goals"))
      )
    ))
  )
)

  
#--------------------------------#
#            Server              #
#--------------------------------#

server <- function(input, output, session) {

  output$hometeam <- renderUI({
    selectInput("hometeam1", "Choose Home Team", data_full[data_full$Div == input$league, "HomeTeam"])
  })
  
  output$awayteam <- renderUI({
    selectInput("awayteam1", "Choose Away Team", data_full[data_full$Div == input$league, "HomeTeam"])
  })
  
  data_goals_app <- reactive({
    if (input$hometeam1 == ""){
      data_goals %>%
        filter(Div %in% input$league)}
    else data_goals %>% 
      filter(HomeTeam %in% input$hometeam1 | HomeTeam %in% input$awayteam1)
    })
  
  ##  output goal table   ##
  output$table_goals <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "share_btts", "goals_per_game",  "share_over0.5HT_total_goals", "share_over1.5HT_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "% of games BTTS", "No. of goals per game", "% of games >0.5 total HT goals", "% of games >1.5 total HT goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("goals_per_game"), 2) %>% 
      formatPercentage(c("share_btts", "share_over0.5HT_total_goals", "share_over1.5HT_total_goals"), 0)
  })
    
}

shinyApp(ui = ui, server = server)