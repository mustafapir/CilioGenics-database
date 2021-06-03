
# Required packages ----

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
#library(webshot)
library(V8)
library(dashboardthemes)
library(waiter)
library(circlize)
library(ComplexHeatmap)
#library(shinyhelper)
library(tippy)
library(Seurat)
#library(dqshiny)
library(highcharter)
library(bsplus)
library(plotly)
library(monocle)


# Sourcing global data and functions ----

source("global.R")
source("functions.R")

# Setting and obtaining cookies ----

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

jscode2 <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#geneName2_search").click();
}});'
# Header functions ----

header <- dashboardHeader(fixed = TRUE, disable = FALSE,
                          dropdownBlock(headerText = "Please cite as following:", id = "drop", title = "How to Cite",
                                        icon = icon("sliders"), badgeStatus = "primary",
                                        type = "messages"))
# anchor <- tags$a(
#   tags$img(
#     src='Logo_browser_ciliogenics.png', height='50', width='50'),
#   'CilioGenics')

header$children[[2]]$children <- tags$div(
  tags$head(
    tags$style(
      HTML(".name { background-color: transparent }"))),
  #anchor,
  class = 'name')


# Ui ----

ui <- shinydashboardPlus::dashboardPage(
  md = TRUE,
  skin = "blue-light",
  header,
  options = list(sidebarExpandOnHover = FALSE),
  ## Sidebar ----
  sidebar = dashboardSidebar(
    minified = TRUE,
    header = singleton(
      tags$head(
        includeHTML(("google-analytics.html"))
        )
      ),
    div(
      id = "logo",
      tags$a(
        tags$img(
          src='Logo_browser_ciliogenics.png', height='40', width='40'),
      ),
      style = "display: inline-block; padding-left: 10px;"
    ),
    div(
      id = "logo-text",
      h4('CilioGenics'),
      style = "display: inline-block; color: #00bcd4; padding-bottom: 10px;"
    ),

    div(
      id = "tabs1",
      sidebarMenu(
        id = "tabs",
        menuItem("Gene search", tabName = "hometab", icon = icon("home")),
        menuItem("Explore data", tabName = "exploretab", icon = icon("search")),
        menuItem("How to use CilioGenics", tabName = "howtab", icon = icon("question-circle")),
        menuItem("Stats", tabName = "statstab", icon = icon("superscript")),
        menuItem("Cite", tabName = "citetab", icon = icon("file-alt")),
        menuItem("About us", tabName = "abouttab", icon = icon("address-card"))
      )
    )
    # div(
    #   id = "darktoggle",
    #   style = "width: 50px; padding: 12px 15px 0px 10px;",
    #   prettyToggle(
    #     inputId = "darkmode",
    #     label_on = "", 
    #     label_off = "",
    #     outline = TRUE,
    #     plain = TRUE,
    #     icon_on = icon("sun"), 
    #     icon_off = icon("moon")
    #   )
    # )
    ),

  ## Body ----
  body = dashboardBody(
    use_waiter(),
    waiter_show_on_load(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    use_cicerone(),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("getcookie", "setcookie", "rmcookie")),
    extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }", functions = "hidehead"),
    mobileDetect('isMobile'),
    ### Tags$head ----
    
    tags$head(
      tags$script(HTML(jscode2)),
      tags$script(src = "js-cookie.js"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-icon-180x180.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      includeCSS("www/stylesheet.css"),
      #uiOutput("dark"),
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

      tags$script("
      Shiny.addCustomMessageHandler('close_drop1', function(x) {
      $('html').click();
                  });"),
      tags$script("
      Shiny.addCustomMessageHandler('close_drop2', function(x) {
      $('html').click();
                  });"),
      tags$script("
    Shiny.addCustomMessageHandler('scsource2', function(value) {
    Shiny.setInputValue('scsource2', value);
    });
  ")
    ),

    ### Menu ----
    

    ### Tabs ----
    tabItems(
      tabItem(
        "hometab",
        
        # div(
        #   id = "toggleui2",
        #   column(
        #     width = 1,
        #     #style = "background-color: #00bcd4; border-radius: 15px 0 0 15px;",
        #     actionButton("toggleSidebar2", icon("th"), style = "padding-top: 20px; padding-bottom: 12px;")
        #   )
        # ),

        navbarPageWithInputs(
          id = "nvbr",
          title = "",
           inputs = searchInput(
            inputId = "geneName2",
            #label = HTML("<h3><center>Gene Search</center></h3>"),
            placeholder = "Search gene",
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "400px",
            value = NULL
          ),
          #tags$script(src = "enter_button2.js"),
        # div(
        #   id = "buttonscicerone",
        #   uiOutput("buttonsui"), #br(), br(),
        #   uiOutput("space")
        # ),
        # br(), br(),br(),br(),br(), br(), br(),
        tabPanel(
          title = "Home",
          fluidRow(
             div(
               id = "landing_page",
              column(
                12,
                tags$script(src = "enter_button.js"),
                align = "center",
                br(), br(), br(), br(), br(), br(), br(),
                HTML("<h1><center><b>WELCOME TO CilioGenics DATABASE</b></center></h1>"),
                br(), br(),
                searchInput(
                  inputId = "geneName",
                  label = HTML("<h3><center><b>Gene Search</b></center></h3>"),
                  placeholder = "Search genes by gene name, gene id or Ensembl gene id",
                  btnSearch = icon("search"),
                  btnReset = icon("remove"),
                  width = "600px",
                  value = NULL
                ),
                div(
                  id = "examples",
                  fluidRow(
                    h5("Identifier examples:"),
                    actionLink("ARL13B","ARL13B"),
                    actionLink("IFT74","ENSG00000096872"),
                    actionLink("BBS5","129880"),
                    actionLink("che-3","che-3"),
                    actionLink("birc5b","birc5b"),
                    actionLink("FBgn0052751","FBgn0052751")
                  )
                ),
                
                HTML("<h3><center>OR</center></h3>"),
                br(),
                actionBttn(
                  inputId = "explore",
                  label = "Explore the gene list",
                  icon = icon("list"),
                  style = "minimal",
                  color = "success"
                )
              )
            )
          )
        ),
        tabPanel(
          title = "General info",
          #uiOutput("searchUI"),
        fluidRow(
          div(
            id = "general_info",
            br(), br(),
            
            box(
              title = "Gene info",
              solidHeader = TRUE,
              status = "success",
              width = 6,
              withSpinner(htmlOutput("textgeneid"), type = 8, color = "#10c891")
            ),
            
            box(
              title = "Rankings in Each Category (Lower is Better)",
              solidHeader = TRUE,
              status = "success",
              width = 6,
              #withSpinner(uiOutput("bargeneinfo"), type = 8, color = "#10c891")
              withSpinner(highchartOutput("polarscores", height = "550px"), type = 8)
            )
          )
        )
        ),
        
        #### Protein interactions ----
        tabPanel(
          title = "Protein interactions",
          #uiOutput("searchUI"),
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
              )
            ),
            br()
          )
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
        )
        ),
        
        #### Phylogenetic analysis ----
        tabPanel(
          title = "Phylogenetic analysis",
          #uiOutput("searchUI"),
        fluidRow(
          div(
            br(), br(),
            id = "cluster_page",
            uiOutput("clusterui"),
            uiOutput("clustertableui")
          )
        )
        ),
        
        #### Single cell ----
        tabPanel(
          title = "Single cell",
          #uiOutput("searchUI"),
        fluidRow(
          div(
            br(),
            id = "sc_cluster_page",
            uiOutput("scInputUI2"),
            uiOutput("scUI2")
            # uiOutput("scclusterui"),
            # uiOutput("scclustertableui")
          )
        )
        ),
        
        #### Publications ----
        tabPanel(
          title = "Publications",
          #uiOutput("searchUI"),
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
                br(),br(), br(),br(), br(),
                uiOutput("pubui")
              )
            )
          )
        )
        ),
        # tags$script(
        #   HTML("var header = $('.navbar > .container-fluid');
        #                       header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"signin\" type=\"button\" class=\"btn btn-primary action-button\" onclick=\"signIn()\">Sign In</button></div>')")
        # ),
        ### Cookie ----
        uiOutput("cookie_footer")
        )
    ),
    ### Explore tab ----
      tabItem(
        "exploretab",
        #uiOutput("toggle"),
        fluidRow(
          div(
            id = "exptab",
            column(
              width = 12,
              #align = "center",
              #offset = 1,
              navbarPage(
                id = "exploredt",
                #theme = "cerulean",
                #width = 12,
                title = "Explore the genes",
                tabPanel(
                  br(), br(),
                  title = "Gene table",
                  id = "tab1",
                  value = "tab1",
                  box(
                    title = "CilioGenics score table",
                    solidHeader = TRUE,
                    status = "success",
                    width = 12,
                    withSpinner(reactableOutput("generaltable2"), type = 8, color = "#10c891")
                  ),
                  br(), br()
                  ),
                tabPanel(
                  id = "tab2",
                  value = "tab2",
                  title = "Phylogenetic Analysis",
                  column(
                    width = 10,
                    offset = 1,
                    box(
                      title = "Phylogenetic analysis",
                      solidHeader = TRUE,
                      status = "success",
                      width = 12,
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
                          withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), type = 8)
                          ),
                        column(
                          width = 12,
                          align = "left",
                          div(
                            id = "button3",
                            style = "left: 10ex;",
                            #dropdown(
                              downloadButton(outputId = "hmap3", label = "Download")
                            #   size = "xm",
                            #   icon = icon("download", class = "opt"),
                            #   up = TRUE
                            # )
                          )
                        )
                      )
                    ),
                  
                    br(),br(),
                    fluidRow(
                      column(
                        width = 12,
                        box(
                          title = "Genes in cluster",
                          solidHeader = TRUE,
                          status = "success",
                          width = 12,
                          withSpinner(reactableOutput("hclusternumbertable"), type = 8)
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  id = "tab3",
                  title = "Single cell clusters",
                  uiOutput("scInputUI"),
                  uiOutput("scUI"),
                  #uiOutput("scUI2")
                  # fluidRow(
                  #   column(
                  #     width = 3,
                  #     pickerInput(
                  #       inputId = "scsource",
                  #       label = "Select source of scRNA-seq data",
                  #       choices = list(
                  #         "Carraro et al(2021) - Lung",
                  #         "Reyfman et al(2018)"
                  #       ),
                  #       selected = NULL,
                  #       multiple = TRUE,
                  #       options = pickerOptions(maxOptions = 1)
                  #     )
                  #   ),
                  #   uiOutput("tippy1"),
                  #   column(
                  #     width = 3,
                  #     uiOutput("scgeneinput") %>% withSpinner(type = 8)
                  #   ),
                  #   uiOutput("tippy2"),
                  #   column(
                  #     width = 3,
                  #     uiOutput("sccelltypeinput") %>% withSpinner(type = 8)
                  #   ),
                  #   uiOutput("tippy3")
                  # ),
                  # fluidRow(
                  #   column(
                  #     width = 4,
                  #     box(
                  #       #title = "Gene list",
                  #       solidHeader = TRUE,
                  #       status = "success",
                  #       width = 12,
                  #       pickerInput(
                  #         inputId = "scsource",
                  #         label = "Select source of scRNA-seq data",
                  #         choices = list(
                  #           "Carraro et al(2021) - Lung",
                  #           "Reyfman et al(2018) - Lung"
                  #         ),
                  #         selected = NULL,
                  #         multiple = TRUE,
                  #         options = pickerOptions(maxOptions = 1)
                  #       ),
                  #       plotOutput("scumapgeneral") %>% withSpinner(type = 8),
                  #       bsTooltip("scsource", "Select a source to visualize the cells", placement = "top")
                  #     )
                  #   ),
                  #   column(
                  #     width = 4,
                  #     box(
                  #       #title = "Gene list",
                  #       solidHeader = TRUE,
                  #       status = "success",
                  #       width = 12,
                  #       uiOutput("scgeneinput") %>% withSpinner(type = 8),
                  #       plotOutput("scmapgene") %>% withSpinner(type = 8),
                  #       bsTooltip("scgeneinput", "Select a gene to display its expression across cells", placement = "top")
                  #     )
                  #   ),
                  #   column(
                  #     width = 4,
                  #     box(
                  #       #title = "Gene list",
                  #       solidHeader = TRUE,
                  #       status = "success",
                  #       width = 12,
                  #       uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
                  #       reactableOutput("scmaptable"),
                  #       bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
                  #     )
                  #   )
                  # )
                ),
                tabPanel(
                  id = "tab4",
                  title = "Publications",
                  # fluidRow(
                  #   column(
                  #     width = 12,
                  #     h4("Please select a gene to visualize associated publications", align = "left"),
                  #     br(),
                  #     h6("Loading gene selector may take a while.", align = "left"),
                  #     uiOutput("pubpickeroutput") %>% withSpinner(type = 8)
                  #     )
                  #   ),
                  # fluidRow(
                  #   uiOutput("pubgeneralheatmapUi")
                  #   ),
                  
                  
                  fluidRow(
                    column(
                      width = 3,
                      box(
                        title = "Gene list",
                        solidHeader = TRUE,
                        status = "success",
                        width = 12,
                        pickerInput(
                          inputId = "pubpub",
                          label = "Select a publication",
                          choices = list(
                            "Publication" = c("", unique(publications$Publication))
                          ),
                          selected = "",
                          options=pickerOptions(liveSearch=T)
                        ),
                      reactableOutput("pubselecttable") %>% withSpinner(type = 8)
                      )
                    ),
                    column(
                      width = 9,
                      box(
                        title = "Number of genes",
                        solidHeader = TRUE,
                        status = "success",
                        width = 12,
                      br(),br(),br(),br(),
                      highchartOutput("pubchart") %>% withSpinner(type = 8)
                      )
                    )
                  )
                )   
              )
            )
          )
        )
      ),
      tabItem(
        "statstab"
        ),
      tabItem(
        "citetab"
        ),
      tabItem(
        "abouttab",
        tags$iframe(src = 'about.html', # put testdoc.html to /www
                    width = '100%', height = '800px', 
                    frameborder = 0, scrolling = 'auto')
        )
    )
  ),
  ## Footer ----
  footer = tags$footer(
    class = "main-footer",
    shiny::tags$div(
      class = "pull-right hidden-xs",
      "By Mustafa S. Pir"),
    tags$a(
      href = "http://kaplanlab.com/",
      tags$img(
        src = "kaplanlab.png",
        style="display: inline-block; vertical-align: top; text-align: center",
        height = "7%",
        width = "7%"
      )
    )
  )
)

