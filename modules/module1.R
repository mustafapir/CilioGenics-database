

geneinfo_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("geneinfo"))
}

geneinfo_server <- function(id, genename){
  moduleServer(
    id,
    function(input, output, session){
      output$geneinfo <- renderUI({
        ns <- session$ns
        HTML(paste("The data shown are for", '<b style="color:#edbc40">', genename(),'</b>', "gene"))
      })
    }
  )
}