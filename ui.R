

library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
#library(formattable)
library(shinycssloaders)
library(reactable)
library(networkD3)
library(iheatmapr)
library(cicerone)
library(data.table)
library(shinyBS)
library(dplyr)

source("global.R")
source("functions.R")


ui <- dashboardPagePlus(
    
   
    header = dashboardHeaderPlus(),
  
    enable_preloader = TRUE,
    collapse_sidebar = TRUE,
    sidebar_background = "light",
    skin = "green",
    
    
    useShinyjs(),
    
    use_cicerone(),
    
    sidebar = dashboardSidebar(
        header = singleton(tags$head(includeHTML(("google-analytics.html")))),
        collapsed = TRUE,
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "hometab", icon = icon("home")),
            menuItem("Gene list", tabName = "exploretab", icon = icon("search"))
        )
    ),
    
    body = dashboardBody(
      
      tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.ico"),
        tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-icon-180x180.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png")
      ),
        
      uiOutput("modalgene"),
      
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
            tags$script(src = "enter_button.js"),
            tags$script("
    Shiny.addCustomMessageHandler('geneName', function(value) {
    Shiny.setInputValue('geneName', value);
    });
  "),
        ),
        
        #uiOutput("buttonsui"),
      
        
      
        br(),
        tabItems(
            tabItem("hometab",
                fluidRow(
                    tags$head(tags$script(src = "enter_button.js")),
                    div(
                        id = "landing_page",
                        tags$head(tags$script(src = "enter_button.js")),
                        column(12,
                            #img(src='Logo_ciliogenics.png', align = "center", width = "250px", height = "250px"),
                            tags$script(src = "enter_button.js"),
                            align = "center",
                            br(), br(),
                            HTML("<h1><center>WELCOME TO <b>CilioGenics</b> DATABASE</center></h1>"),
                            br(), br(),
                            
                            searchInput(
                                inputId = "geneName",
                                label = HTML("<h4><center>Search a gene name</center></h4>"),
                                placeholder = "Type a name",
                                btnSearch = icon("search"),
                                btnReset = icon("remove"),
                                width = "40%",
                                value = NULL
                            ),
                            HTML("<h3><center>OR</center></h3>"),
                            br(),
                            actionBttn(
                                inputId = "explore",
                                label = "Explore the gene list",
                                icon = icon("list"),
                                style = "minimal",
                                color = "success")
                        )
                    )
                )
            ),
      
            
            tabItem("exploretab",
            
                    # fluidRow(
                    #   div(
                    #     id = "tabButtons2",
                    #     column(
                    #       width = 12,
                    #       align = "center",
                    #       br(),
                    #       actionBttn("homePage1", "Home",
                    #                  icon = icon("home"), 
                    #                  style = "unite",
                    #                  color = "default",
                    #                  size = "sm"),
                    #       actionBttn("generalPage1", "General Information",
                    #                  icon = img(src = "network2.png", height = "20px"), 
                    #                  color = "success",
                    #                  style = "unite",
                    #                  size = "sm")
                    #     )
                    #   )
                    # ),
                    
                    fluidRow(
                        div(
                            id = "exptab",
                        tabBox(
                            id = "exploredt",
                            width = 12,
                            title = "Explore the genes",
                            tabPanel(
                                br(), br(),
                                title = "Gene table",
                                id = "tab1",
                                withSpinner(reactableOutput("generaltable2"), color = "#10c891"),
                                br(), br(),
                                div(title = "Gene order by categories",
                                withSpinner(reactableOutput("generaltable3"), color = "#10c891"))
                                #textOutput("ot")
                            ),
                            tabPanel(
                                id = "tab2",
                                title = "Clusters",
                                pickerInput(
                                    inputId = "clusternumber",
                                    label = "Select cluster number to explore", 
                                    choices = list(
                                        "Ciliary organisms specific clusters" = c(56,58),
                                        "Average conservation" = c(1,4,6,9,16,30),
                                        "Low specificity" = c(2:3,5,7:8,10:15,17:29,31:55,57,58:60)
                                        )
                                ),
                                fluidRow(
                                  column(
                                    width = 9,
                                    withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), color = "#10c891")
                                  ),
                                  column(
                                    width = 3,
                                    withSpinner(reactableOutput("hclusternumbertable"), color = "#10c891")
                                  )
                                )
                  
                            )
                        )
                    )
                    )
            )
        ),
        
        # fluidRow(
        #     div(
        #       br(), br(),
        #       
        #       id = "generalinfo",
        #       
        #     )
        # ),
        
        fluidRow(
          div(
            br(), br(),
            
            id = "general_info",
            boxPlus(
              title = "Gene info",
              solidHeader = TRUE,
              status = "success",
              #background = "purple",
              width = 12,
              htmlOutput("textgeneid")
            ),
            
            boxPlus(
              
              progressBar(id = "pb8", value = 21271, total = 21271, status = "info", display_pct = TRUE, striped = TRUE, title = "Overall percentile")
              
            )
            
          )
        ),
        
        
        
        fluidRow(
            div(
                br(), br(),
            
                id = "protein_interaction",
                column(
                    width = 3,
                    
                    checkboxGroupButtons(
                        inputId = "proradio",
                        label = "Select the source of interaction: ",
                        choices = c("Biogrid" = "Biogrid", "Intact" = "Intact", "Wormbase" = "Wormbase"),
                        selected = c("Biogrid", "Intact", "Wormbase"),
                        justified = TRUE, status = "success",
                        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                        direction = "vertical",
                        individual = TRUE
                    ),
                ), br(),
                # column(
                #     width = 3,
                #     switchInput(
                #         label = "Switch on only if total number of interaction is less than 4",
                #         inputId = "pro_children"
                #     )
                # )
            ),
        ),
        fluidRow(
            div(
                id = "protein_interaction1",
                column(
                    width = 6,
                    uiOutput("pro_box1")
                ),
                column(
                    width = 6,
                    uiOutput("protable")
                )
                #column(
                #    width = 12,
                #    uiOutput("pro_box2")
            )
        ),
        
        
        
        fluidRow(
            div(
                br(), br(),
                id = "pub",
                column(
                    width = 6,
                    boxPlus(
                      title = "List of publications",
                      solidHeader = TRUE,
                      status = "success",
                      reactableOutput("pubtable")
                    )
                )
            )
        ),
        
        fluidRow(
            div(
                br(), br(),
                id = "cluster_page",
                uiOutput("clusterui"),
                uiOutput("clustertableui")
            )
        ),
        
        fluidRow(
            div(
                br(), br(),
                column(
                    width = 1,
                    id = "back_button",
                    tags$div(
                        title = "click to go back",
                        actionBttn(
                            inputId = "back",
                            label = "BACK", 
                            style = "material-flat",
                            color = "success",
                            icon = icon("arrow-left"),
                            size = "sm"
                        )
                    )
                )
            )
        )
        
    )
)