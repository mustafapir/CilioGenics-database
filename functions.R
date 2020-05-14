
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
  )