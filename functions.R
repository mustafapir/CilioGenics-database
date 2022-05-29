
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
  div(br(),
      tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
      value,
      tippy(bsButton("tooltipbutton", label = "", icon = icon("question"), style = "info", size = "extra-small"),
            sprintf("<span style='font-size:15px;'>%s<span>", tooltip), placement = "top", animation = "scale", arrow = TRUE, theme = "blue")
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
    el = "tabs1",
    title ="Gene list",
    description ="You can toggle between gene search and gene list from side menu."
  )
#       "You can click a gene to further explore in detail. The numbers on second table represent order of gene in that column."

guide2 <- Cicerone$
  new()$
  step(
    "nvbr",
    "Navigation bar",
    "Here, you can toggle between different tabs. Each tab consists of various information and plots about the gene"
  )$
  step(
    "general_info_box",
    "Gene info",
    "Here you can find various information about the gene"
  )$
  step(
    "score_box",
    "CilioGenics scores for each category",
    "The plot shows CilioGenics score for different categories that can be further explored in other tabs."
  )

guide3 <- Cicerone$
  new()$
  step(
    "proradio",
    "Select source",
    "Here you can select and deselect the source to filter results. By default, all sources are selected."
  )$
  step(
    "protein_box1",
    "Protein interaction network",
    "This plot shows the proteins that gene of interest is interacting with. You can see the name of protein by hovering mouse over that dot."
  )$
  step(
    "protein_box2",
    "Protein interaction table",
    "The table shows the same interactions, with more information. You can search gene or other information on the table. You can also
    download the table by clicking download button."
  )

guide4<- Cicerone$
  new()$
  step(
    "heatmapcluster",
    "Heatmap",
    "The genes in this heatmap are the genes that are in the same cluster with searched gene"
  )$
  step(
    "button1",
    "Toggle button",
    "Although heatmap of a particular cluster gives a general idea about all genes in that cluster, you can still see orthology information
    of only the searched gene. For this, click this button to switch between that gene and whole cluster."
  )$
  step(
    "button2",
    "Download heatmap",
    "You can download heatmap by clicking this button. Please note that download may take time."
  )$
  step(
    "cluster_box",
    "Gene list",
    "Here is the list of all genes which appear in this cluster."
  )

guide5<- Cicerone$
  new()$
  step(
    "scsource2",
    "Single cell source",
    "You can select a single cell source to visualize more information about expression profile of gene of interest. For more information
     about sources used here, please refer to 'Source' page."
  )

guide6<- Cicerone$
  new()$
  step(
    "publication_box",
    "Publications",
    "Here shows the publications that the gene of interest is mentioned."
  )

guide7<- Cicerone$
  new()$
  step(
    "motif_box",
    "Motifs",
    "The motif information is from MotifMap. We have mapped motifs to genome to find the genes that have those motifs in their promoter."
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


