library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(Homo.sapiens)
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
library(webshot)
library(V8)
library(dashboardthemes)
library(waiter)
library(circlize)
library(ComplexHeatmap)
#library(shinyhelper)
library(tippy)


#library(rintrojs)

source("global.R")
source("functions.R")
#webshot::install_phantomjs()

#addResourcePath("js", "www")

jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params));  
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'
# header = shinydashboardPlus::dashboardHeader(
#   fixed = TRUE,
#   disable = FALSE
# )
# header$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
#                                    tags$img(src='Logo_ciliogenics.png', height = "30%", width = "30%"))

header <- dashboardHeader(fixed = TRUE, disable = FALSE)
anchor <- tags$a(
                 tags$img(src='Logo_browser_ciliogenics.png', height='50', width='50'),
                 'CilioGenics')

header$children[[2]]$children <- tags$div(
  tags$head(tags$style(HTML(".name { background-color: transparent }"))),
  anchor,
  class = 'name')

ui <- shinydashboardPlus::dashboardPage(
  md = TRUE,
  skin = "blue-light",
  #skin = "midnight",
  # header = shinydashboardPlus::dashboardHeader(
  #   fixed = TRUE,
  #   disable = FALSE
  # ),
  header,
  # enable_preloader = TRUE,
  # collapse_sidebar = TRUE,
  # sidebar_background = "light",
  # skin = "green",
  #header,
  options = list(sidebarExpandOnHover = TRUE),
  sidebar = dashboardSidebar(
    minified = TRUE,
    header = singleton(tags$head(includeHTML(("google-analytics.html")))),
    
    div(
      id = "tabs1",
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "hometab", icon = icon("home")),
        menuItem("Gene list", tabName = "exploretab", icon = icon("search")),
        menuItem("How to use CilioGenics", tabName = "howtab", icon = icon("question-circle")),
        menuItem("Stats", tabName = "statstab", icon = icon("superscript")),
        menuItem("Cite", tabName = "citetab", icon = icon("file-alt")),
        menuItem("About us", tabName = "abouttab", icon = icon("address-card"))
      )
    )
  ),
  
  body = dashboardBody(
    
    
    # shinyDashboardThemes(
    #   theme = "grey_dark"
    # ),
    use_waiter(),
    #waiter_show_on_load(html = spin_3(), color = "#333e48", logo = ""),
    waiter_show_on_load(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    use_cicerone(),
    #customTheme,
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("getcookie", "setcookie", "rmcookie")),
    tags$head(
      tags$script(src = "js-cookie.js")
    ),
    
    
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-icon-180x180.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png")
    ),
    
    uiOutput("modalgene"),
    textOutput("sometextop"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      includeCSS("www/stylesheet.css"),
      tags$script(src = "enter_button.js"),
      tags$script("
    Shiny.addCustomMessageHandler('geneName', function(value) {
    Shiny.setInputValue('geneName', value);
    });
  "),
      tags$script("
    Shiny.addCustomMessageHandler('hmapEvent', function(value) {
    Shiny.setInputValue('hmapEvent', value);
    });
  "),
      
      tags$script("Shiny.addCustomMessageHandler('close_drop1', function(x){
                  $('html').click();
                });"),
      tags$script("Shiny.addCustomMessageHandler('close_drop2', function(x){
                  $('html').click();
                });")
    ),
    div(
      id = "buttonscicerone",
      uiOutput("buttonsui"), #br(), br(),
      uiOutput("space")),
    
    br(), br(),
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
                           label = HTML("<h3><center>Gene Search</center></h3>"),
                           placeholder = "Search genes by gene name, gene id or Ensembl gene id",
                           btnSearch = icon("search"),
                           btnReset = icon("remove"),
                           width = "600px",
                           value = NULL
                         ),
                         
                         HTML("<h3><center>OR</center></h3>"),
                         br(),
                         
                         
                         actionBttn(
                           inputId = "explore",
                           label = "Explore the gene list",
                           icon = icon("list"),
                           style = "minimal",
                           color = "success"),
                         
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
                  column(
                    width = 10,
                    align = "center",
                    offset = 1,
                    tabBox(
                      id = "exploredt",
                       width = 12,
                       title = "Explore the genes",
                       
                      tabPanel(
                        br(), br(),
                        title = "Gene table",
                        id = "tab1",
                        value = "tab1",
                        # helper(shiny::actionButton("go", "click me!"),
                        #        icon = "exclamation",
                        #        colour = "red",
                        #        type = "markdown",
                        #        content = "ClickHelp"),
                        withSpinner(reactableOutput("generaltable2"), type = 8, color = "#10c891"),
                        
                        br(), br(),
                        div(title = "Gene order by categories",
                            withSpinner(reactableOutput("generaltable3"), type = 8, color = "#10c891"))
                        #textOutput("ot")
                      ),
                      
                      tabPanel(
                        id = "tab2",
                        value = "tab2",
                        title = "Phylogenetic Analysis",
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
                            width = 12,
                            withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), type = 8, color = "#10c891")
                          ),
                          column(
                            width = 12,
                            withSpinner(reactableOutput("hclusternumbertable"), type = 8, color = "#10c891")
                          )
                        )
                        
                      ),
                      
                      tabPanel(
                        id = "tab3",
                        title = "Single cell clusters",
                        fluidRow(
                          column(
                            width = 3,
                            pickerInput(
                              inputId = "clusternumber2",
                              label = "Select cluster number to explore", 
                              choices = list(
                                "Ciliary cells specific clusters" = 5,
                                "Neurons specific cluster" = 7,
                                "Low specificity" = c(1:4,6,8:20)
                              ),
                              selected = 5
                            )
                          ),
                          column(
                            width = 3,
                            uiOutput("pickeroutput")
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            withSpinner(iheatmaprOutput("scheatmapclusternumber", height = "600px"), type = 8, color = "#10c891")
                          ),
                          column(
                            width = 12,
                            withSpinner(reactableOutput("schclusternumbertable"), type = 8, color = "#10c891")
                          )
                        )
                        
                      ),
                      tabPanel(
                        id = "tab4",
                        title = "Publications",
                        fluidRow(
                          withSpinner(iheatmaprOutput("pubgeneralheatmap", height = "1000px"), type = 8)
                        )
                      )
                      
                    )
                  )
                )
              )
      ),
      
      tabItem("statstab"
      ),
      tabItem("citetab"
      ),
      tabItem("abouttab"
      )
    ),
    
    
    fluidRow(
      div(
        br(), br(),
        
        id = "general_info",
        box(
          title = "Gene info",
          solidHeader = TRUE,
          status = "success",
          #background = "purple",
          width = 6,
          withSpinner(htmlOutput("textgeneid"), type = 8, color = "#10c891")
        ),
        
        box(
          title = "Rankings in Each Category (Lower is Better)",
          solidHeader = TRUE,
          status = "success",
          width = 6,
          withSpinner(uiOutput("bargeneinfo"), type = 8, color = "#10c891"),
          withSpinner(uiOutput("bargeneinfo1"), type = 8, color = "#10c891"),
          uiOutput("bargeneinfo2"),
          uiOutput("bargeneinfo3"),
          uiOutput("bargeneinfo4"),
          uiOutput("bargeneinfo5"),
          uiOutput("bargeneinfo6"),
          uiOutput("bargeneinfo7")
          
        )
        
      )
    ),
    
    
    
    fluidRow(
      div(
        style="margin-left:15px",
        br(),
        
        id = "protein_interaction",
        column(
          width = 6,
          
          checkboxGroupButtons(
            inputId = "proradio",
            label = "Select the source of interaction: ",
            choices = c("Biogrid" = "Biogrid", "Intact" = "Intact", "Wormbase" = "Wormbase"),
            selected = c("Biogrid", "Intact", "Wormbase"),
            justified = TRUE, status = "info",
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
      )
    ),
    
    
    
    fluidRow(
      div(
        br(), br(),
        id = "pub",
        column(
          width = 10,
          offset = 1,
          align = "center",
          box(
            width = 12,
            title = "List of publications",
            solidHeader = TRUE,
            status = "success",
            br(),br(),br(),
            plotOutput("pubheatmap"),
            #iheatmaprOutput("pubheatmap"),
            br(),br(), br(),br(), br(),
            uiOutput("pubui")
          )
        )
      )
    ),
    
    fluidRow(
      div(
        br(), br(),
        id = "single_cell",
        column(
          width = 12,
          box(
            width = 12,
            title = "Single Cell Plot",
            solidHeader = TRUE,
            status = "success",
            plotOutput("scHeatmap", height = "1200px")
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            reactableOutput("scTable")
          )
        )
      )
    ),
    
    
    fluidRow(
      div(
        br(), br(),
        id = "sc_cluster_page",
        uiOutput("scclusterui"),
        uiOutput("scclustertableui")
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
    
    
    # fluidRow(
    #   div(
    #     
    #     column(
    #       width = 1,
    #       id = "back_button",
    #       tags$div(
    #         title = "click to go back",
    #         actionBttn(
    #           inputId = "back",
    #           label = "BACK", 
    #           style = "material-flat",
    #           color = "success",
    #           icon = icon("arrow-left"),
    #           size = "sm"
    #         )
    #       )
    #     )
    #   )
    # ),
    
    uiOutput("cookie_footer")
    #tags$footer(cookie_box)
    # tags$footer(class = "main-footer", style = "margin-bottom: 0; bottom: 0; position: fixed; width: 10%", 
    #             tags$img(src = "kaplanlab.png", height = "100%", width = "100%"))
    
  ),
   
  footer = tags$footer(class = "main-footer", shiny::tags$div(class = "pull-right hidden-xs",
                                                              "By Mustafa S. Pir"),
                       tags$a(href = "http://kaplanlab.com/", tags$img(src = "kaplanlab.png", 
                                                                       style="display: inline-block; vertical-align: top; text-align: center", 
                                                                       height = "7%", width = "7%")))
  # footer = tags$footer(class = "main-footer", shiny::tags$div(class = "pull-right hidden-xs", 
  #                                                             "KaplanLab, 2021"), "By Mustafa S. Pir", 
  #                      HTML('<center><img src="kaplanlab.png" width="8%"></center>'))
  
  # footer = dashboardFooter(
  #   left = "By Mustafa S. Pir",
  #   right = "KaplanLab, 2021",
  #   img(src='kaplanlab.png', align = "center")
  # )
)