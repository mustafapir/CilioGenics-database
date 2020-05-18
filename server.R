
library(shiny)

source("functions.R")


server <- function(input, output, session) { 
    
    addClass(selector = "body", class = "sidebar-collapse")
    
    
     observeEvent("", {
         
         showModal(modalDialog(
             includeHTML("intro.html"),
             easyClose = TRUE,
             footer = tagList(
                 actionButton(inputId = "tour", label = "Introductory Tour", icon = icon("info-circle")),
                 actionButton(inputId = "close", label = "Close") #icon = icon(""))
             )
         ))
         
         show("landing_page")
         hide("protein_interaction")
         hide("protein_interaction1")
         hide("tabButtons")
         hide("pub")
         hide("cluster_page")
         
     }, once = TRUE)
     
     observeEvent(input$tour, {
         removeModal()
         guide$init()$start()
     })
     
    # observeEvent(input$geneName, {
    #     show("protein_interaction")
    #     hide("landing_page")
    #     show("tabButtons")
    # })
     
     observeEvent(input$close,{
         removeModal()
     })
    
    
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
         else if (!(toupper(genename()) %in% toupper(gene_synonyms2$Gene_name))){
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
             show("protein_interaction1")
             hide("landing_page")
             show("tabButtons")
             hide("cluster_page")
             hide("pub")
             show("back_button")
             click("proteinPage")
             #hide("proteinPage")
             updateTabItems(session, "tabs", selected = character(0))
         }
         
     },
     ignoreInit = TRUE,
     ignoreNULL = TRUE
     )
    observeEvent(input$homePage, {
         show("landing_page")
         hide("protein_interaction")
         hide("protein_interaction1")
         hide("tabButtons")
         hide("pub")
         hide("cluster_page")
         click("geneName_reset")
         hide("back_button")
         updateTabItems(session, "tabs", "hometab")
    })
    
    observeEvent(input$proteinPage, {
        show("protein_interaction")
        show("protein_interaction1")
        hide("landing_page")
        hide("pub")
        hide("cluster_page")
        #hide("proteinPage")
    })
    
    observeEvent(input$pubPage, {
        show("pub")
        hide("landing_page")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("cluster_page")
        #hide("pubPage")
        #show("proteinPage")
    })
    
    observeEvent(input$clusterPage, {
        show("cluster_page")
        hide("landing_page")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("pub")
        #hide("clusterPage")
        #show("proteinPage")
    })
    
    observeEvent(input$close,{
        removeModal()
    })
    
    observeEvent(input$hometab, {
        show("landing_page")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("tabButtons")
        hide("cluster_page")
        hide("pub")
        hide("back_button")
        
    })
    
    observeEvent(input$tabs == "exploretab", {
        show("exploretab")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("tabButtons")
        hide("pub")
        show("exptab")
        hide("cluster_page")
        hide("back_button")
        #updateTabItems(session, "tabs", "exploretab")
    })
    
    observeEvent(input$tabs == "hometab", {
        show("landing_page")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("tabButtons")
        hide("pub")
        hide("cluster_page")
        hide("back_button")
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
                show("protein_interaction1")
                hide("landing_page")
                show("back_button")
                click("proteinPage")
                updateTabItems(session, "tabs", selected = character(0))
            #click("geneName_search")
            }
        }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
    
    
    observeEvent(genenumber2(), {
        if (length(genenumber2() != "") != 0){
            if (genenumber2() != ""){
                session$sendCustomMessage("geneName", genenumber2())
                #updateTabItems(session, "tabs", "hometab")
                hide("exptab")
                show("tabButtons")
                show("protein_interaction")
                show("protein_interaction1")
                hide("landing_page")
                show("back_button")
                click("proteinPage")
                updateTabItems(session, "tabs", selected = character(0))
                #click("geneName_search")
            }
        }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
    
    
    observeEvent(input$back, {
        
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("tabButtons")
        hide("pub")
        hide("cluster_page")
        hide("back_button")
        show("exptab")
        show("landing_page")
    })
    
    
    genename<-reactive({
        
        if (!(toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_name)) && 
            (toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_synonyms)) && 
            length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName)])) == 1) {
            gname<-gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName))]
        }
        else if (toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_name)){
            gname<-unique(gene_synonyms2$Gene_name[which(gene_synonyms2$Gene_name %in% toupper(input$geneName))])
            }
        else {gname<-input$geneName}
        gname
    })
    
    genenumber<-reactive({
        final_score_table$Gene_name[selected()]
    })
    
    genenumber2<-reactive({
        final_score_table$Gene_name[selected2()]
    })
    
    
    
    networkdata<-reactive({
        
        a<-biogrid %>% filter(toupper(Gene_name_A) == toupper(genename()))
        b<-intact %>% filter(toupper(Gene_name_A) == toupper(genename()))
        c<-wbP %>% filter(toupper(Gene_name_A) == toupper(genename()))
        zz<-data.frame(rbind(a, b, c))#[which(data.frame(rbind(a, b, c))$Gene_name_A %in% input$proradio),]
        colnames(zz)<-c("Interactor A", "Interactor B", "Source")
        zz[which(zz$Source %in% input$proradio),]
        
    })
    
    pubdata<-reactive({
        
        data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))]))
    })
    
    inputcluster<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == aa$cluster_number[which(toupper(aa$Gene_name) == toupper(genename()))]),2:73])
    })
    
    inputclusternumber<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == input$clusternumber),2:73])
    })
    
    inputclustertable<-reactive({
        
        data.frame('Gene name' = nscores2[which(aa$cluster_number == aa$cluster_number[which(toupper(aa$Gene_name) == toupper(genename()))]),1])
    })
    
    inputclusternumbertable<-reactive({
        
        data.frame('Gene_name' = nscores2[which(aa$cluster_number == input$clusternumber),1])
    })
    
    
    selected <- reactive(getReactableState("generaltable2", "selected"))
    selected2 <- reactive(getReactableState("generaltable3", "selected"))
    
    output$networkplot<-renderSimpleNetwork({
        simpleNetwork(networkdata(), height = "200px", width = "200px", zoom = TRUE,
                      opacity = 1, fontSize = 8)
    })
    
    output$pro_int<-renderReactable({
        
        reactable(networkdata())
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
                                                         format = colFormat(digits = 3)),
                      Protein_atlas_score = colDef(name = "Protein atlas", 
                                                 format = colFormat(digits = 3)),
                      Total_score = colDef(name = "Raw score", 
                                                 format = colFormat(digits = 3)),
                      Norm_total_score = colDef(name = "Normalized score", 
                                                 format = colFormat(digits = 3)),
                      Publication_score = colDef(name = "Publications", 
                                                 format = colFormat(digits = 3)),
                      Weighted_total_scores = colDef(name = "Weighted score", 
                                                     format = colFormat(digits = 3))
                  
                      ),
                  selection = "single",
                  onClick = "select"
        )
            
    })
    
    output$generaltable3<-renderReactable({
        
        reactable(final_seq_table, resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
                  columns =list(
                      Protein_interaction_score = colDef(name = "Protein interactions", 
                                                         format = colFormat(digits = 0)),
                      Genetic_interaction_score = colDef(name = "Genetic interactions", 
                                                         format = colFormat(digits = 0)),
                      Single_cell_score = colDef(name = "Single cell", 
                                                 format = colFormat(digits = 0)),
                      Cluster_score = colDef(name = "Clusters", 
                                             format = colFormat(digits = 0)),
                      Motif_score = colDef(name = "Motifs", 
                                           format = colFormat(digits = 0)),
                      Publication_score = colDef(name = "Publications", 
                                                 format = colFormat(digits = 0)),
                      Protein_atlas_score = colDef(name = "Protein atlas", 
                                                   format = colFormat(digits = 0)),
                      Total_score = colDef(name = "Raw score", 
                                           format = colFormat(digits = 0)),
                      Norm_total_score = colDef(name = "Normalized score", 
                                                format = colFormat(digits = 0)),
                      Publication_score = colDef(name = "Publications", 
                                                 format = colFormat(digits = 0)),
                      Weighted_total_scores = colDef(name = "Weighted score", 
                                                     format = colFormat(digits = 0))
                      
                  ),
                  selection = "single",
                  onClick = "select"
        )
        
    })
    
    output$textForPro<-renderText({paste("There is no protein interaction for", genename(), "gene")})
    #output$ot<-renderText({paste("this is", genename())})
    
        output$pro_box1 <- renderUI({
            
                box(
                    title = paste0("Protein interaction network for ", genename(), " gene"),
                    width = 12,
                    if (length(networkdata()[[1]]) == 0){
                        #print(paste("There is no protein interaction for", genename(), "gene"))
                        textOutput("textForPro")
                    }
                    else {withSpinner(simpleNetworkOutput("networkplot"))}
                )
        })
    
    output$pubtable<-renderReactable({
        reactable(pubdata(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
        })
    
    output$heatmapcluster<-renderIheatmap({
        
        main_heatmap(inputcluster(), layout = list(paper_bgcolor='transparent'))%>%
            add_row_labels(size = 0.05,font = list(size = 7))%>%
            add_col_labels(size = 0.6, font = list(family = "Open Sans", size = 14), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })
    
    output$hclustertable<-renderReactable({
        
        reactable(inputclustertable(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
    })
    
    output$hclusternumbertable<-renderReactable({
        
        reactable(inputclusternumbertable(), resizable = TRUE, filterable = TRUE,
                  defaultPageSize = 10, showPageSizeOptions = TRUE)
    })
    
    output$heatmapclusternumber<-renderIheatmap({
        
        main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'))%>%
            add_row_labels(size = 0.05,font = list(size = 7))%>%
            add_col_labels(size = 0.6,font = list(family = "Open Sans", size = 14), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })

    
}
