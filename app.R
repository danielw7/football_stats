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
  
  theme = shinytheme("superhero"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      (""),
      br(),
      
      #------------------------------------------------------------------------------------#
      #            add select buttons for league, home team and away team                  #
      #------------------------------------------------------------------------------------#
      
      selectInput("league", "Choose League", choices = data_full$Div, selected = c(" England - Premier League")),
      
      uiOutput("hometeam"), # use uiOutput since teams should depend on selected leage
      
      checkboxGroupInput("pitch", label = "Playing Site",
                         choices = c("all", "home", "away"), selected = "all")),
    
    mainPanel(tabsetPanel(
      
      tabPanel("Goals",
               br(),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Goals"                              #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Full Time Goals",
                          br(), br(),
                          htmlOutput("text_total_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_ft")), br(), br(),
                          htmlOutput("text_team_goals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_ft")), br(), br()),
                 
                 tabPanel("Half Time Goals",
                          br(), br(),
                          htmlOutput("text_total_firsthalf_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_firsthalf")), br(), br(),
                          htmlOutput("text_firsthalf_teamgoals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_firsthalf")), br(), br(),
                          htmlOutput("text_total_secondhalf_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_secondhalf")), br(),br(),
                          htmlOutput("text_secondhalf_teamgoals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_secondhalf"))),
                 
                 tabPanel("BTTS",
                          br(),
                          (DT::dataTableOutput("table_goals_btts")))
               )
      )
    ))
  )
)

  
#--------------------------------#
#            Server              #
#--------------------------------#

server <- function(input, output, session) {

  #------------------------------------------------------------------------------------#
  #                 make team selection dependend on league selection                  #
  #------------------------------------------------------------------------------------#
  
  output$hometeam <- renderUI({
    selectizeInput("hometeam1", label = "Choose Home Team", choices = data_full[data_full$Div == input$league, "HomeTeam"],
                   multiple = TRUE, options = list(placeholder = "Select Teams"))
  })
  
  #------------------------------------------------------------------------------------#
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_goals_app <- reactive({
    if (is.null(input$hometeam1)){
      data_goals %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_goals %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #                        add table for total full time goals                         #
  #------------------------------------------------------------------------------------#
  
  output$text_total_goals <- renderText({
    print(paste0("<b>Match Goals</b>"))
  })
  
  output$table_goals_ft <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "goals_per_game",  "share_over0.5FT_total_goals", "share_over1.5FT_total_goals", 
                         "share_over2.5FT_total_goals", "share_over3.5FT_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average goals per game", "% of games >0.5 match goals", "% of games >1.5 match goals",
                     "% of games >2.5 match goals", "% of games >3.5 match goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("goals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5FT_total_goals", "share_over1.5FT_total_goals", "share_over2.5FT_total_goals", 
                         "share_over3.5FT_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                        add table for full time team goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_team_goals <- renderText({
    print(paste0("<b>Team Goals</b>"))
  })
  
  output$table_teamgoals_ft <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "teamgoals_per_game", "share_over0.5FT_team_goals", "share_over1.5FT_team_goals", 
                         "share_over2.5FT_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average goals per game", "% of games >0.5 team goals", "% of games >1.5 team goals", 
                     "% of games >2.5 team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("teamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5FT_team_goals", "share_over1.5FT_team_goals", "share_over2.5FT_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for first half match goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_total_firsthalf_goals <- renderText({
    print(paste0("<b>First Half Match Goals</b>"))
  })
  
  output$table_goals_firsthalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "firsthalfgoals_per_game", "share_over0.5HT_total_goals", 
                         "share_over1.5HT_total_goals", "share_over2.5HT_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average first half goals per game", "% of games >0.5 first half goals", 
                     "% of games >1.5 first half goals", "% of games >2.5 first half  goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("firsthalfgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5HT_total_goals", "share_over1.5HT_total_goals", "share_over2.5HT_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for first half team goals                           #
  #------------------------------------------------------------------------------------#
  
  output$text_firsthalf_teamgoals <- renderText({
    print(paste0("<b>First Half Team Goals</b>"))
  })
  
  output$table_teamgoals_firsthalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "firsthalfteamgoals_per_game", "share_over0.5HT_team_goals", 
                         "share_over1.5HT_team_goals", "share_over2.5HT_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average first half team goals per game", "% of games >0.5 first half team goals", 
                     "% of games >1.5 first half team goals", "% of games >2.5 first half team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("firsthalfteamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5HT_team_goals", "share_over1.5HT_team_goals", 
                         "share_over2.5HT_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                       add table for second half match goals                        #
  #------------------------------------------------------------------------------------#
  
  output$text_total_secondhalf_goals <- renderText({
    print(paste0("<b>Second Half Match Goals</b>"))
  })
  
  output$table_goals_secondhalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "secondhalfgoals_per_game", "share_over0.5secondhalf_total_goals", 
                         "share_over1.5secondhalf_total_goals", "share_over2.5secondhalf_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average second half goals per game", "% of games >0.5 second half goals", 
                     "% of games >1.5 second half goals", "% of games >2.5 second half goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("secondhalfgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5secondhalf_total_goals", "share_over1.5secondhalf_total_goals",
                         "share_over2.5secondhalf_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for second half team goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_secondhalf_teamgoals <- renderText({
    print(paste0("<b>Second Half Team Goals</b>"))
  })
  
  output$table_teamgoals_secondhalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "secondhalfteamgoals_per_game", "share_over0.5secondhalf_team_goals", 
                         "share_over1.5secondhalf_team_goals", "share_over2.5secondhalf_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Average second half team goals per game", 
                     "% of games >0.5 second half team goals", "% of games >1.5 second half team goals", 
                     "% of games >2.5 second half team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatRound(columns = c("secondhalfteamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5secondhalf_team_goals", "share_over1.5secondhalf_team_goals",
                         "share_over2.5secondhalf_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                                 add table for btts                                 #
  #------------------------------------------------------------------------------------#
  
  output$table_goals_btts <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "share_btts")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "% of games both teams scored"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(width = "150px", targets = "_all"))), rownames = FALSE) %>%
      formatPercentage(c("share_btts"), 0)
  })
  
}

shinyApp(ui = ui, server = server)