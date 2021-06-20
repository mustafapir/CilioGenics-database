
library(cicerone)
library(reactable)

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", id = "inpt", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

with_tooltip <- function(value, tooltip, ...) {
  div(br(),br(),
    tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
     tippy(bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
           sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
           ),
     br(),
     value
  )
}

with_tooltip2 <- function(value, tooltip, ...) {
  #span(style = "text-decoration: underline; text-decoration-style: dotted;", title = tooltip, value)
  #bsButton("q1", label = tooltip, icon = icon("question"), style = "info", size = "extra-small")
  div(br(),
      tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
      value,
      #bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
      tippy(bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
            sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", animation = "scale", arrow = TRUE, theme = "blue")
      #tippy_this("tooltipbutton", sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", allowHTML = TRUE, theme = "light")
  )
}

with_tooltip3 <- function(value, tooltip, ...) {
  div(
    tags$style("display: flex; flexDirection: column; justifyContent: center; text-align: center;"),
        value,
        tippy(bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
              sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", animation = "scale", arrow = TRUE, theme = "blue",
              trigger = "click")


  )
}

with_tooltip4 <- function(value, tooltip, ...) {
  div(
    tags$style("display: flex; flexDirection: column; justifyContent: center; text-align: center;"),
    value,
    tippy(bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
          sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", animation = "scale", arrow = TRUE, theme = "blue")


  )
}


guide <- Cicerone$
  new()$
  step(
    el = "geneName",
    title = "Gene Input",
    description = "You can search by gene name, NCBI gene ID, Ensembl ID and gene synonyms."
  )$
  step(
    "explore",
    "Explore gene list",
    "Alternatively, you can explore the gene list and clusters."
  )$
  step(
    "tabs1",
    "Gene list",
    "You can toggle between gene search and gene list from side menu."
  )
#       "You can click a gene to further explore in detail. The numbers on second table represent order of gene in that column."

guide2 <- Cicerone$
  new()$
  step(
    "nvbr",
    "Navigation bar",
    "Here, you can toggle between different tabs"
  )$
  step(
    "general_info",
    "Gene info",
    "Here are all the info about the gene "
  )



genebar<-function (id1, value1, title1, lowlimit, highlimit){
  if (value1 <= lowlimit){
    a<-progressBar(id = id1, value = value1, total = 21271, status = "success", display_pct = TRUE, striped = TRUE, title = title1)
  }
  else if (value1 > lowlimit && value1 <= highlimit){
    a<-progressBar(id = id1, value = value1, total = 21271, status = "info", display_pct = TRUE, striped = TRUE, title = title1)
  }
  else {a<-progressBar(id = id1, value = value1, total = 21271, status = "danger", display_pct = TRUE, striped = TRUE, title = title1)}
  return(a)
}

sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo29"

downloadFunction <- function(id) {
  div(
    style = "position: absolute; left: 6em;bottom: 0.5em;",
    dropdown(
      downloadButton(outputId = id, label = "Download plot"),
      size = "xm",
      icon = icon("download", class = "opt"),
      up = TRUE
    )
  )
}


orange_pal <- function(x){
  if (!is.na(x)){
    rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)
  } else {
    "#e9e9e9" #grey
  }
}



# customTheme <- shinyDashboardThemeDIY(
#   ### general
#   appFontFamily = "open_sansregular"
#   ,appFontColor = "rgb(0,0,0)"
#   ,primaryFontColor = "rgb(0,0,0)"
#   ,infoFontColor = "rgb(0,0,0)"
#   ,successFontColor = "rgb(0,0,0)"
#   ,warningFontColor = "rgb(0,0,0)"
#   ,dangerFontColor = "rgb(0,0,0)"
#   ,bodyBackColor = "rgb(248,248,248)"
#
#   ### header
#   ,logoBackColor = "transparent"
#
#   ,headerButtonBackColor = "rgb(238,238,238)"
#   ,headerButtonIconColor = "rgb(75,75,75)"
#   ,headerButtonBackColorHover = "rgb(210,210,210)"
#   ,headerButtonIconColorHover = "rgb(0,0,0)"
#
#   ,headerBackColor = "rgb(238,238,238)"
#   ,headerBoxShadowColor = "#aaaaaa"
#   ,headerBoxShadowSize = "2px 2px 2px"
#
#   ### sidebar
#   ,sidebarBackColor = cssGradientThreeColors(
#     direction = "down"
#     ,colorStart = "rgb(20,97,117)"
#     ,colorMiddle = "rgb(56,161,187)"
#     ,colorEnd = "rgb(3,22,56)"
#     ,colorStartPos = 0
#     ,colorMiddlePos = 50
#     ,colorEndPos = 100
#   )
#   ,sidebarPadding = 0
#
#   ,sidebarMenuBackColor = "transparent"
#   ,sidebarMenuPadding = 0
#   ,sidebarMenuBorderRadius = 0
#
#   ,sidebarShadowRadius = "3px 5px 5px"
#   ,sidebarShadowColor = "#aaaaaa"
#
#   ,sidebarUserTextColor = "rgb(255,255,255)"
#
#   ,sidebarSearchBackColor = "rgb(55,72,80)"
#   ,sidebarSearchIconColor = "rgb(153,153,153)"
#   ,sidebarSearchBorderColor = "rgb(55,72,80)"
#
#   ,sidebarTabTextColor = "rgb(255,255,255)"
#   ,sidebarTabTextSize = 13
#   ,sidebarTabBorderStyle = "none none solid none"
#   ,sidebarTabBorderColor = "rgb(35,106,135)"
#   ,sidebarTabBorderWidth = 1
#
#   ,sidebarTabBackColorSelected = cssGradientThreeColors(
#     direction = "right"
#     ,colorStart = "rgba(44,222,235,1)"
#     ,colorMiddle = "rgba(44,222,235,1)"
#     ,colorEnd = "rgba(0,255,213,1)"
#     ,colorStartPos = 0
#     ,colorMiddlePos = 30
#     ,colorEndPos = 100
#   )
#   ,sidebarTabTextColorSelected = "rgb(0,0,0)"
#   ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
#
#   ,sidebarTabBackColorHover = cssGradientThreeColors(
#     direction = "right"
#     ,colorStart = "rgba(44,222,235,1)"
#     ,colorMiddle = "rgba(44,222,235,1)"
#     ,colorEnd = "rgba(0,255,213,1)"
#     ,colorStartPos = 0
#     ,colorMiddlePos = 30
#     ,colorEndPos = 100
#   )
#   ,sidebarTabTextColorHover = "rgb(50,50,50)"
#   ,sidebarTabBorderStyleHover = "none none solid none"
#   ,sidebarTabBorderColorHover = "rgb(75,126,151)"
#   ,sidebarTabBorderWidthHover = 1
#   ,sidebarTabRadiusHover = "0px 20px 20px 0px"
#
#   ### boxes
#   ,boxBackColor = "rgb(255,255,255)"
#   ,boxBorderRadius = 5
#   ,boxShadowSize = "0px 1px 1px"
#   ,boxShadowColor = "rgba(0,0,0,.1)"
#   ,boxTitleSize = 16
#   ,boxDefaultColor = "rgb(210,214,220)"
#   ,boxPrimaryColor = "rgba(44,222,235,1)"
#   ,boxInfoColor = "rgb(210,214,220)"
#   ,boxSuccessColor = "rgb(210,214,220)"
#   #,boxSuccessColor = "rgba(0,255,213,1)"
#   ,boxWarningColor = "rgb(244,156,104)"
#   ,boxDangerColor = "rgb(255,88,55)"
#
#   ,tabBoxTabColor = "rgb(255,255,255)"
#   ,tabBoxTabTextSize = 14
#   ,tabBoxTabTextColor = "rgb(0,0,0)"
#   ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
#   ,tabBoxBackColor = "rgb(255,255,255)"
#   ,tabBoxHighlightColor = "rgba(44,222,235,1)"
#   ,tabBoxBorderRadius = 5
#
#   ### inputs
#   ,buttonBackColor = "rgb(245,245,245)"
#   ,buttonTextColor = "rgb(0,0,0)"
#   ,buttonBorderColor = "rgb(200,200,200)"
#   ,buttonBorderRadius = 5
#
#   ,buttonBackColorHover = "rgb(235,235,235)"
#   ,buttonTextColorHover = "rgb(100,100,100)"
#   ,buttonBorderColorHover = "rgb(200,200,200)"
#
#   ,textboxBackColor = "rgb(255,255,255)"
#   ,textboxBorderColor = "rgb(200,200,200)"
#   ,textboxBorderRadius = 5
#   ,textboxBackColorSelect = "rgb(245,245,245)"
#   ,textboxBorderColorSelect = "rgb(200,200,200)"
#
#   ### tables
#   ,tableBackColor = "rgb(255,255,255)"
#   ,tableBorderColor = "rgb(240,240,240)"
#   ,tableBorderTopSize = 1
#   ,tableBorderRowSize = 1
#
# )

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

bar_style <- function(width = 1, fill = "#e6e6e6", height = "30%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color,
    display = "flex",
    flexDirection = "column",
    justifyContent = "center"
  )
}


bar_style2 <- function(width = 1, fill = "#e6e6e6", height = "70%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color,
    display = "flex",
    flexDirection = "column",
    justifyContent = "center"
  )
}

load_data_mysql <- function() {
  dbConnect(MySQL(), dbname = "ciliogenics",
            host = Sys.getenv("host"),
            port = 3306,
            user = Sys.getenv("user"),
            password = Sys.getenv("password"))
}


