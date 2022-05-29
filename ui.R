
# Required packages ----

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(Homo.sapiens)
library(RMySQL)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(reactable)
library(networkD3)
library(iheatmapr)
library(cicerone)
library(data.table)
library(shinyBS)
library(dplyr)
library(V8)
library(dashboardthemes)
library(waiter)
library(circlize)
library(ComplexHeatmap)
library(tippy)
library(Seurat)
library(highcharter)
library(bsplus)
library(plotly)
library(monocle)


# Sourcing global data and functions ----

source("global.R")
source("functions.R")
source("modules/module1.R")

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

jscode3 <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'

jscode4 <- "shinyjs.init = function() {
  window.onpopstate = function (event) {
    Shiny.onInputChange('navigatedTo', location.search);
  } 
}
shinyjs.updateHistory = function(params) {
  var queryString = [];
  for (var key in params) {
    queryString.push(encodeURIComponent(key) + '=' + encodeURIComponent(params[key]));
  }
  queryString = '?' + queryString.join('&');
  history.pushState(null, null, queryString)
}"

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
  title = "Welcome to CilioGenics database",
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
    
    # Logo
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
    
    # Menu
    div(
      id = "tabs1",
      sidebarMenu(
        id = "tabs",
        menuItem("Gene search", tabName = "hometab", icon = icon("home")),
        menuItem("Explore data", tabName = "exploretab", icon = icon("search")),
        menuItem("How to use CilioGenics", tabName = "howtab", icon = icon("question-circle")),
        menuItem("Data", tabName = "datatab", icon = icon("database"),
                 menuSubItem("Source","sourcetab",icon = icon("file")),
                 menuSubItem("Download", "downloadtab", icon = icon("download"))
        ),
        menuItem("About", tabName = "abouttab", icon = icon("address-card")),
        actionBttn(
          inputId = "helpbutton",
          label = "Help",
          icon = icon("question-circle"),
          style = "bordered",
          color = "primary",
          size = "sm"
        )
      )
    ),
    
    # toggle for dark mode
    div(
      id = "darktoggle",
      style = "width: 50px; padding: 12px 15px 0px 10px;",
      prettyToggle(
        inputId = "darkmode",
        label_on = "",
        label_off = "",
        outline = TRUE,
        plain = TRUE,
        icon_on = icon("sun"),
        icon_off = icon("moon")
      )
    )
  ),
  
  ## Body ----
  body = dashboardBody(
    use_waiter(),
    waiter_show_on_load(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
    use_cicerone(),
    useShinyjs(),
    extendShinyjs(text = jscode4, functions = c("updateHistory")),
    extendShinyjs(text = jsCode, functions = c("getcookie", "setcookie", "rmcookie")),
    extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }", functions = "hidehead"),
    mobileDetect('isMobile'),
    
    ### Tags$head ----
    tags$head(
      tags$script(HTML(jscode2)),
      tags$script(jscode3),
      tags$script(src = "js-cookie.js"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-icon-180x180.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      includeCSS("www/stylesheet.css"),
      
      # JS event scripts for communication with shiny
      tags$script("
      Shiny.addCustomMessageHandler('geneName', function(value) {
      Shiny.setInputValue('geneName', value);
      });"),
      
      tags$script("
      Shiny.addCustomMessageHandler('hmapEvent', function(value) {
      Shiny.setInputValue('hmapEvent', value);
      });"),
      
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
      });"),
      
      # Dark mode UI
      uiOutput("dark"),
    ),
    
    ### Menu ----
    
    
    ### Tabs ----
    tabItems(
      
      #### Gene search tab items ----
      tabItem(
        "hometab",
        # custom navbarpage
        navbarPageWithInputs(
          id = "nvbr",
          title = "",
          
          inputs = searchInput(
            inputId = "geneName2",
            placeholder = "Search gene",
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "400px",
            value = NULL
          ),
          
          ##### Home tab ----
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
                  
                  # example search inputs
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
          
          ##### General info tab ----
          tabPanel(
            title = "General info",
            h3(geneinfo_ui("ginfo")),
            
            fluidRow(
              div(
                id = "general_info",
                br(),
                box(
                  id = "general_info_box",
                  title = "Gene info",
                  solidHeader = TRUE,
                  status = "success",
                  width = 6,
                  htmlOutput("textgeneid") %>% withSpinner(type = 8)
                )
              ),
              
              div(
                id = "scoreinfo",
                box(
                  id = "score_box",
                  title = "CilioGenics scores for each category",
                  solidHeader = TRUE,
                  status = "success",
                  width = 6,
                  uiOutput("polarUI") %>% withSpinner(type = 8),
                  h5("* Integrated normalized CilioGenics scores."),
                  h5("* Motif score represents the occurrence of RFX1 and MIF-1 motifs
                     around the 1000 bp upstream region of searched gene."),
                  h5("* Phylogeny score is based on the cluster the searched gene belongs to."),
                  h5("* Protein and genetic interaction scores are based on the rate of interactions
                     with ciliary proteins/genes by all interactions"),
                  h5("")
                )
              )
            )
          ),
          
          ##### Protein interactions ----
          tabPanel(
            title = "Interactions",
            h3(geneinfo_ui("ginfo1")),
            
            # interaction source checkboxes
            fluidRow(
              div(
                style="margin-left:15px",
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
            
            # interaction network plot
            h3("Protein interactions"),
            br(),
            fluidRow(
              div(
                id = "protein_interaction1",
                column(
                  width = 6,
                  box(
                    id = "protein_box1",
                    title = "Network plot for protein interactions",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    uiOutput("pro_box1") %>% withSpinner(type = 8)
                  )
                )
              ),
              
              # protein interaction table
              div(
                id = "protein_interaction1_tbl",
                column(
                  width = 6,
                  box(
                    id = "protein_box2",
                    width = 12,
                    title = "Protein interactions",
                    solidHeader = TRUE,
                    status = "success",
                    reactableOutput("pro_int") %>% withSpinner(type = 8),
                    downloadButton(outputId = "prot.table", label = "Download Table")
                  )
                )
              )
            ),
            
            # Genetic interaction plot and table
            h3("Genetic interactions"),
            br(),
            fluidRow(
              div(
                id = "genetic_interaction1",
                column(
                  width = 6,
                  box(
                    title = "Network plot for genetic interactions",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    uiOutput("gene_box") %>% withSpinner(type = 8)
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = 12,
                    title = "Genetic interactions",
                    solidHeader = TRUE,
                    status = "success",
                    reactableOutput("gene_int") %>% withSpinner(type = 8),
                    downloadButton(outputId = "gene.table", label = "Download Table")
                  )
                )
              )
            )
          ),
          
          ##### Phylogenetic analysis ----
          tabPanel(
            title = "Phylogenetic analysis",
            h3(geneinfo_ui("ginfo2")),
            fluidRow(
              div(
                br(),
                id = "cluster_page",
                uiOutput("clusterui"),
                uiOutput("clustertableui")
              )
            )
          ),
          
          ##### Single cell ----
          tabPanel(
            title = "Single cell",
            fluidRow(
              div(
                br(),
                id = "sc_cluster_page",
                uiOutput("scInputUI2"),
                uiOutput("scUI2")
              )
            )
          ),
          
          ##### Publications ----
          tabPanel(
            title = "Publications",
            h3(geneinfo_ui("ginfo3")),
            
            fluidRow(
              div(
                br(), br(),
                id = "pub",
                column(
                  width = 10,
                  offset = 1,
                  align = "center",
                  box(
                    id = "publication_box",
                    width = 12,
                    title = "List of publications",
                    solidHeader = TRUE,
                    status = "success",
                    uiOutput("pubexpui"),
                    br(),br(),br(),
                    plotOutput("pubheatmap") %>% withSpinner(type = 8),
                    br(),br(), br(),br(), br(),
                    uiOutput("pubui") %>% withSpinner(type = 8)
                  )
                )
              )
            )
          ),
          
          ##### Motifs ----
          tabPanel(
            title = "Motifs",
            h3(geneinfo_ui("ginfo4")),
            fluidRow(
              div(
                br(), br(),
                id = "motif",
                column(
                  width = 10,
                  offset = 1,
                  box(
                    id = "motif_box",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    title = "Motifs",
                    h4("The table below shows the motifs that appears in the promoter (1000bp upstream) region of
                       the gene of interest."),
                    reactableOutput("motiftbl")
                  )
                )
              )
            )
          ),
          
          #### Cookie ----
          uiOutput("cookie_footer")
        )
      ),
      
      #### Explore tab items ----
      tabItem(
        "exploretab",
        
        fluidRow(
          div(
            id = "exptab",
            column(
              width = 12,
              
              navbarPage(
                id = "exploredt",
                title = "Explore the genes",
                
                ##### Gene table ----
                tabPanel(
                  br(),
                  title = "Gene table",
                  id = "tab1",
                  value = "tab1",
                  box(
                    title = "CilioGenics score table",
                    solidHeader = TRUE,
                    status = "success",
                    width = 12,
                    h4("This is an extensive list of all human genes included in the database.
                       The column headers provide the description of the data, giving a score for each gene.
                       Users can type a gene name and sort columns.", style = "margin-left: 50px;"),
                    h5("* Table shows weighted scores for all categories. Weights are calculated based on
                       each section's capability to find ciliary genes.", style = "margin-left:50px;"),
                    withSpinner(reactableOutput("generaltable2"), type = 8),
                    h5("Click the genes to easily switch to its gene search page.")
                  ),
                  br(), br()
                ),
                
                ##### Phylogenetic analysis ----
                tabPanel(
                  id = "tab2",
                  value = "tab2",
                  title = "Phylogenetic Analysis",
                  
                  uiOutput("heatmapclusternumberUI"),
                  uiOutput("clustergenetable"),
                  br(),br()
                ),
                
                ##### Single cell ----
                tabPanel(
                  id = "tab3",
                  title = "Single cell clusters",
                  uiOutput("scInputUI"),
                  uiOutput("scUI"),
                ),
                
                ##### Publications ----
                tabPanel(
                  id = "tab4",
                  title = "Publications",
                  
                  fluidRow(
                    div(
                      h3(tags$b("Explore a compilation of 52 papers that presents the list of
                                ciliary and putative ciliary genes."),
                         style = "margin-left: 50px;"),
                      br(),
                      column(
                        width = 6,
                        box(
                          width = 12,
                          solidHeader = TRUE,
                          status = "success",
                          title = "List of publications",
                          h4("To explore the data sets,
                                choose the article of interest from the table below."),
                          br(style = "line-height: 0.1"),
                          h5("* To open the paper on Pubmed instead, click title of the paper."),
                          reactableOutput("pubselecttable2") %>% withSpinner(type = 8)
                        )
                      ),
                      uiOutput("publicationUI1")
                    )
                  ),
                  
                  fluidRow(
                    uiOutput("publicationUI2")
                  )
                ),
                
                ##### Motifs ----
                tabPanel(
                  id = "tab5",
                  title = "Motifs",
                  box(
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    title = "Motifs",
                    column(
                      width = 12,
                      offset = 1,
                      h4(tags$b("Explore the motifs and the genes with these motifs in their promoter.")),
                    ),
                    fluidRow(
                      column(
                        width = 3,
                        offset = 1,
                        uiOutput("motifUI")
                      ),
                      column(
                        width = 2,
                        offset = 1,
                        uiOutput("consUI")
                      )
                    ),
                    br(),
                    fluidRow(
                      uiOutput("motiftableUI")
                    )
                  )
                ),
                
                ##### Protein atlas ----
                tabPanel(
                  id = "tab6",
                  title = "Protein Atlas",
                  div(
                    box(
                      width = 12,
                      title = "Protein Atlas",
                      solidHeader = TRUE,
                      status = "success",
                      h4("List of genes in the Human Protein Atlas database", tags$a(src="https://www.proteinatlas.org/"),
                         " is compiled. The following keywords were used in compilation: ",
                         tags$b("Cilia, Cilium, Centrosome, Flagella, Flagellum.")),
                      h5("* Comment column shows the text where the keyword appears."),
                      reactableOutput("proAtlas")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      
      #### How to use CilioGenics ----
      tabItem(
        "howtab",
        box(
          solidHeader = FALSE,
          width = 12,
          includeHTML("www/howto.html")
        )
      ),
      
      #### About tab ----
      tabItem(
        "abouttab",
        tags$iframe(src = 'about.html',
                    width = '100%', height = '1200px',
                    frameborder = 0, scrolling = 'auto')
      ),
      
      #### Source tab ----
      tabItem(
        "sourcetab",
        br(),
        box(
          title = "Source list used in CilioGenics",
          width = 12,
          solidHeader = TRUE,
          status = "success",
          br(),
          column(
            width = 12,
            h4(tags$b("Molecular & Genetic Interactions")),
            
            reactableOutput("sourcetable"),
            br(),br(),
            h4(tags$b("Single Cell RNA-Seq")),
            reactableOutput("sourcetable1"),
            br(),br(),
            h4(tags$b("Motifs")),
            reactableOutput("sourcetable2"),
            br(),br(),
            h4(tags$b("Others")),
            reactableOutput("sourcetable3"),
            br(),br()
          )
        )
      ),
      
      #### Download tab ----
      tabItem(
        "downloadtab",
        br(),
        box(
          title = "Downloads for data used in CilioGenics",
          width = 12,
          solidHeader = TRUE,
          status = "success",
          br(),
          column(
            width = 12,
            h4(tags$b("Single Cell RNA-Seq")),
            reactableOutput("downloadtable2"),
            br(),br(),
            h4(tags$b("Motifs")),
            reactableOutput("downloadtable3"),
            br(),br(),
            h4(tags$b("Moleculer & Genetic Interactions")),
            reactableOutput("downloadtable4"),
            br(),br(),
            h4(tags$b("HGNC Gene Info")),
            reactableOutput("downloadtable5"),
            br(),br(),
            h4(tags$b("Orthology Data")),
            reactableOutput("downloadtable6"),
            br(),br(),
            h4(tags$b("Sequence Data Used in Phylogenetic Analysis")),
            reactableOutput("downloadtable1"),
            br(),br()
          )
        )
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
    ),
    tags$h5("CilioGenics",br(),"Ver 0.1.0", style = "display: inline-block; margin-left: 40%; margin-top: 1px;")
  )
)

