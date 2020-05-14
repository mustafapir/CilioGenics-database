#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

source("global.R")

# Define UI for application that draws a histogram

ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(),
    
    use_cicerone(),
    sidebar = dashboardSidebar(
        
        sidebarMenu(
            id = "tabs",
            menuItem("Home", tabName = "hometab", icon = icon("home")),
            menuItem("Gene list", tabName = "exploretab", icon = icon("search"))
        )
    ),
    body = dashboardBody(
        tags$head(
            #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            tags$script(src = "enter_button.js"),
            tags$script("
    Shiny.addCustomMessageHandler('geneName', function(value) {
    Shiny.setInputValue('geneName', value);
    });
  "),
        ),
        useShinyjs(),
        fluidRow(
            div(
                id = "tabButtons",
                column(
                    width = 12,
                    align = "center",
                    br(),
                    bsButton("homePage", "Home",
                             icon = icon("home"), type = "action",
                             style = "default", value = TRUE),
                    bsButton("proteinPage", "Protein interactions",
                             icon = icon("toggle-off"), type = "action",
                             style = "primary", value = TRUE),
                    # bsButton("showpanel8", "Genetic interactions",
                    #          icon = icon("toggle-off"), type = "action",
                    #          style = "primary", value = TRUE),
                    # bsButton("showpanel8", "Single cell",
                    #          icon = icon("toggle-off"), type = "action",
                    #          style = "primary", value = TRUE),
                    bsButton("clusterPage", "Clusters",
                             icon = icon("toggle-off"), type = "action",
                             style = "primary", value = TRUE),
                    # bsButton("showpanel8", "Motifs",
                    #          icon = icon("toggle-off"), type = "action",
                    #          style = "primary", value = TRUE),
                    bsButton("pubPage", "Publications",
                             icon = icon("toggle-off"), type = "action",
                             style = "primary", value = TRUE),
                    # bsButton("showpanel8", "Protein atlas",
                    #          icon = icon("toggle-off"), type = "action",
                    #          style = "primary", value = TRUE)
                    
                )
            )
        ),
        
        tabItems(
            tabItem("hometab",
                fluidRow(
                    tags$head(tags$script(src = "enter_button.js")),
                    div(
                        id = "landing_page",
                        tags$head(tags$script(src = "enter_button.js")),
                        column(12,
                            tags$script(src = "enter_button.js"),
                            align = "center",
                            br(), br(), br(), br(),
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
                                color = "primary"),
                            #textOutput("ot")
                        )
                    )
                )
            ),
    
            
            tabItem("exploretab",
            
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
                                withSpinner(reactableOutput("generaltable2")),
                                textOutput("ot")
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
                                withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"))
                            )
                        )
                    )
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
                        justified = TRUE, status = "primary",
                        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                        direction = "vertical",
                        individual = TRUE
                    ),
                ),
                column(
                    width = 12,
                    uiOutput("pro_box1")
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
                    reactableOutput("pubtable")
                )
            )
        ),
        
        fluidRow(
            div(
                br(), br(),
                id = "cluster_page",
                column(
                    width = 6,
                    withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "600px"))
                )
            )
        )
        
    )
)