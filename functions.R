
library(cicerone)


guide <- Cicerone$
  new()$ 
  step(
    el = "geneName",
    title = "Gene Input",
    description = "This is where you enter a gene name to explore"
  )$
  step(
    "explore",
    "Explore gene list",
    "Instead of searching a gene, here you can explore our gene list with scores."
  )#$
  # step(
  #   "exploredt",
  #   "Gene list",
  #   "You can click a gene to further explore in detail. The numbers on second table represent order of gene in that column."
  # )

  genebar<-function (id1, value1, title1){
    if (value1 <= 1169){
      a<-progressBar(id = id1, value = value1, total = 21271, status = "success", display_pct = TRUE, striped = TRUE, title = title1)
    }
    else if (value1 > 1169 && value1 <= 1934){
      a<-progressBar(id = id1, value = value1, total = 21271, status = "info", display_pct = TRUE, striped = TRUE, title = title1)
    }
    else {a<-progressBar(id = id1, value = value1, total = 21271, status = "danger", display_pct = TRUE, striped = TRUE, title = title1)}
    return(a)
  }
