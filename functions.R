
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
