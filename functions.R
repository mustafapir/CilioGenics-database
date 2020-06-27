
library(cicerone)


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

guide1 <- Cicerone$
  new()$
  step(
    "homePage",
    "Gene table",
    "You can toggle between gene table and clusters"
  )$
  step(
    "tab1",
    "Tables",
    "This is a list of all genes. You can find "
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
  