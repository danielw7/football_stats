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
      
      checkboxGroupInput("pitch", label = "Stats by Playing Site",
                         choices = c("all", "home", "away"), selected = "all"),
    
      uiOutput("referee")), # use uiOutput since teams should depend on selected leage
    
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
                          br(), br(),
                          htmlOutput("text_btts"), br(),br(),
                          (DT::dataTableOutput("table_goals_btts")))
               )
      ),
      
      tabPanel("Cards",
               br(),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Cards"                              #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Total Cards",
                          br(), br(),
                          htmlOutput("text_match_cards"), br(),br(),
                          (DT::dataTableOutput("table_match_cards"))),
                          
                 tabPanel("Team Cards",
                          br(), br(),
                          htmlOutput("text_team_cards"), br(),br(),
                          (DT::dataTableOutput("table_team_cards")))
               )
      ),
      
      tabPanel("Corners",
               br(),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Corners"                            #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Total Corners",
                          br(), br(),
                          htmlOutput("text_match_corners"), br(),br(),
                          (DT::dataTableOutput("table_match_corners"))),
                          
                 tabPanel("Team Corners",
                          br(), br(),
                          htmlOutput("text_team_corners"), br(),br(),
                          (DT::dataTableOutput("table_team_corners")))
                 
               )
      ), 
      
      tabPanel("Referees",
               br(), br(),
               htmlOutput("text_ref1"), br(),br(),
               (DT::dataTableOutput("table_referees1")), br(), br(),
               htmlOutput("text_ref2"), br(),br(),
               (DT::dataTableOutput("table_referees2")))
                 
                
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
    selectizeInput("hometeam1", label = "Choose Team(s)", choices = data_full[data_full$Div == input$league, "HomeTeam"],
                   multiple = TRUE, options = list(placeholder = "Select Teams"))
  })
  
  #------------------------------------------------------------------------------------#
  #             make referee selection (not dependend on league selection)             #
  #------------------------------------------------------------------------------------#
  
  output$referee <- renderUI({
    selectizeInput("referee1", label = "Choose Referee(s)", choices = data_ref[, "Referee"],
                   multiple = TRUE, options = list(placeholder = "Select Referees"))
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
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_cards_app <- reactive({
    if (is.null(input$hometeam1)){
      data_cards %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_cards %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_corners_app <- reactive({
    if (is.null(input$hometeam1)){
      data_corners %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_corners %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #         reactive expression to make referee table react on selections              #
  #------------------------------------------------------------------------------------#
  
  data_ref_app <- reactive({
    if (is.null(input$referee1)){
      data_ref} 
    else data_ref %>%
        filter(Referee %in% input$referee1)
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2), visible = TRUE, width = "80"),
                                         list(targets = c(3:6), visible = TRUE, width = "110"))),
                                         rownames = FALSE) %>%
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2), visible = TRUE, width = "80"),
                                         list(targets = c(3:5), visible = TRUE, width = "110"))),
        rownames = FALSE) %>%
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2:5), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2:5), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2:5), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
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
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2:5), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
      formatRound(columns = c("secondhalfteamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5secondhalf_team_goals", "share_over1.5secondhalf_team_goals",
                         "share_over2.5secondhalf_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                                 add table for btts                                 #
  #------------------------------------------------------------------------------------#
  
  output$text_btts <- renderText({
    print(paste0("<b>Share of games where both teams scored</b>"))
  })
  
  output$table_goals_btts <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "share_btts")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "% of games both teams scored"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2), visible = TRUE, width = "110"))), 
        rownames = FALSE) %>%
      formatPercentage(c("share_btts"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                               add table for referees 1                             #
  #------------------------------------------------------------------------------------#
  
  output$text_ref1 <- renderText({
    print(paste0("<b>Cards per game and share of games with red cards</b>"))
  })
  
  output$table_referees1 <- DT::renderDataTable({
    data_ref_app()[, c("Referee", "total_cards_per_game", "yellow_cards_per_game", "red_cards_per_game", 
                       "share_over0.5_red_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Referee", "Average total cards per game", "Average yellow cards per game", 
                     "Average red cards per game", "% of games with >0.5 red cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:4), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatRound(columns = c("total_cards_per_game", "yellow_cards_per_game", "red_cards_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5_red_cards"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                               add table for referees 1                             #
  #------------------------------------------------------------------------------------#
  
  output$text_ref2 <- renderText({
    print(paste0("<b>Share of games with yellow cards</b>"))
  })
  
  output$table_referees2 <- DT::renderDataTable({
    data_ref_app()[, c("Referee", "share_over0.5_yellow_cards", "share_over1.5_yellow_cards", 
                       "share_over2.5_yellow_cards", "share_over3.5_yellow_cards", 
                       "share_over4.5_yellow_cards", "share_over5.5_yellow_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Referee", "% of games with >0.5 yellow cards", "% of games with >1.5 yellow cards", 
                     "% of games with >2.5 yellow cards", "% of games with >3.5 yellow cards",
                     "% of games with >4.5 yellow cards", "% of games with >5.5 yellow cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:6), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatPercentage(c("share_over0.5_yellow_cards", "share_over1.5_yellow_cards", 
                         "share_over2.5_yellow_cards", "share_over3.5_yellow_cards", 
                         "share_over4.5_yellow_cards", "share_over5.5_yellow_cards"), 0)
  })
  
}

shinyApp(ui = ui, server = server)