#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("functions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) { 
    
    
    
    guide$init()$start()
    
     observeEvent("", {
         show("landing_page")
         hide("protein_interaction")
         hide("tabButtons")
         hide("pub")
         hide("cluster_page")
         
     }, once = TRUE)
     
    # observeEvent(input$geneName, {
    #     show("protein_interaction")
    #     hide("landing_page")
    #     show("tabButtons")
    # })
    
    
     observeEvent(input$geneName_search, { 
          if (genename() == ""){
              showModal(modalDialog(
                  "Please first write a gene name",
                  easyClose = TRUE,
                  footer = tagList(
                      actionButton(inputId = "close", label = "Close", icon = icon("info-circle"))
                  )
                  )
              )
          }
         else if (!(toupper(input$geneName) %in% toupper(final_score_table$Gene_name))){
             sendSweetAlert(
                 session = session,
                 title = "ERROR!",
                 text = "Please check the gene name and try again",
                 type = "error",
                 showCloseButton = TRUE
             )
             #click("geneName_reset")
         }
         else {
             show("protein_interaction")
             hide("landing_page")
             show("tabButtons")
             click("proteinPage")
         }
         
     },
     ignoreInit = TRUE,
     ignoreNULL = TRUE
     )
    observeEvent(input$homePage, {
         show("landing_page")
         hide("protein_interaction")
         hide("tabButtons")
         hide("pub")
         hide("cluster_page")
         click("geneName_reset")
         updateTabItems(session, "tabs", "hometab")
    })
    
    observeEvent(input$proteinPage, {
        show("protein_interaction")
        hide("landing_page")
        hide("pub")
        hide("cluster_page")
    })
    
    observeEvent(input$pubPage, {
        show("pub")
        hide("landing_page")
        hide("protein_interaction")
        hide("cluster_page")
    })
    
    observeEvent(input$clusterPage, {
        show("cluster_page")
        hide("landing_page")
        hide("protein_interaction")
        hide("pub")
    })
    
    observeEvent(input$close,{
        removeModal()
    })
    
    observeEvent(input$hometab, {
        show("landing_page")
        hide("protein_interaction")
        hide("tabButtons")
        hide("cluster_page")
        hide("pub")
        
    })
    
    observeEvent(input$tabs == "exploretab", {
        show("exploretab")
        hide("protein_interaction")
        hide("tabButtons")
        hide("pub")
        show("exptab")
        #updateTabItems(session, "tabs", "exploretab")
    })
    
    observeEvent(input$tabs == "hometab", {
        show("landing_page")
        hide("protein_interaction")
        hide("tabButtons")
        hide("pub")
        click("geneName_reset")
    })
    
    observeEvent(input$explore, {
        
        updateTabItems(session, "tabs", "exploretab")
    })
    
    observeEvent(genenumber(), {
        if (length(genenumber() != "") != 0){
            if (genenumber() != ""){
                session$sendCustomMessage("geneName", genenumber())
                #updateTabItems(session, "tabs", "hometab")
                hide("exptab")
                show("tabButtons")
                show("protein_interaction")
                hide("landing_page")
            #click("geneName_search")
            }
        }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
    
    
    genename<-reactive({
        input$geneName
    })
    
    genenumber<-reactive({
        final_score_table$Gene_name[selected()]
    })
    
    
    
    networkdata<-reactive({
        
        a<-biogrid %>% filter(toupper(Gene_name_A) == toupper(genename()))
        b<-intact %>% filter(toupper(Gene_name_A) == toupper(genename()))
        c<-wbP %>% filter(toupper(Gene_name_A) == toupper(genename()))
        zz<-data.frame(rbind(a, b, c))#[which(data.frame(rbind(a, b, c))$Gene_name_A %in% input$proradio),]
        zz[which(zz$type %in% input$proradio),]
        
    })
    
    pubdata<-reactive({
        
        data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(input$geneName))]))
    })
    
    inputcluster<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == aa$cluster_number[which(toupper(aa$Gene_name) == toupper(input$geneName))]),2:73])
    })
    
    inputclusternumber<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == input$clusternumber),2:73])
    })
    
    selected <- reactive(getReactableState("generaltable2", "selected"))
    
    
    output$networkplot<-renderSimpleNetwork({
        simpleNetwork(networkdata(), height = "200px", width = "200px", zoom = TRUE,
                      opacity = 1, fontSize = 8)
    })
        
    output$generaltable2<-renderReactable({
            
        reactable(final_score_table, resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
                  columns =list(
                      Protein_interaction_score = colDef(name = "Protein interactions", 
                                                         format = colFormat(digits = 3)),
                      Genetic_interaction_score = colDef(name = "Genetic interactions", 
                                                         format = colFormat(digits = 3)),
                      Single_cell_score = colDef(name = "Single cell", 
                                                         format = colFormat(digits = 3)),
                      Cluster_score = colDef(name = "Clusters", 
                                                         format = colFormat(digits = 3)),
                      Motif_score = colDef(name = "Motifs", 
                                                         format = colFormat(digits = 3)),
                      Publication_score = colDef(name = "Publications", 
                                                         format = colFormat(digits = 3))
                  
                      ),
                  selection = "single",
                  onClick = "select"
        )
            
    })
    
    output$textForPro<-renderText({paste("There is no protein interaction for", genename(), "gene")})
    output$ot<-renderText({paste("this is", input$geneName)})
    
        output$pro_box1 <- renderUI({
            div(
                style = "position: relative",
                
                boxPlus(
                    title = paste0("Protein interaction network for ", genename(), " gene"),
                    closable = TRUE,
                    collapsible = TRUE,
                    enable_dropdown = FALSE,
                    if (length(networkdata()[[1]]) == 0){
                        #print(paste("There is no protein interaction for", genename(), "gene"))
                        textOutput("textForPro")
                    }
                    else {withSpinner(simpleNetworkOutput("networkplot"))}
                )
            )
        })
    
    output$pubtable<-renderReactable({
        reactable(pubdata()
                  )}
        )
    
    output$heatmapcluster<-renderIheatmap({
        
        main_heatmap(inputcluster(), layout = list(paper_bgcolor='transparent'))%>%
            add_row_labels(size = 0.05,font = list(size = 7))%>%
            add_col_labels(size = 0.6,font = list(size = 9), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })
    
    output$heatmapclusternumber<-renderIheatmap({
        
        main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'))%>%
            add_row_labels(size = 0.05,font = list(size = 7))%>%
            add_col_labels(size = 0.6,font = list(size = 9), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })

    
}
