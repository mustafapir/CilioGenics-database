
library(shiny)

source("functions.R")
options(reactable.theme = reactableTheme(
    #backgroundColor = "CadetBlue",
    #borderColor = "SteelBlue",
    
))

GnYlRd <- function(x) rgb(colorRamp(c("#63be7b", "#ffeb84", "#f87274"))(x), maxColorValue = 255)

server <- function(input, output, session) { 
    
    
     observeEvent("", {
         
         showModal(modalDialog(
             includeHTML("intro.html"),
             easyClose = TRUE,
             footer = tagList(
                 actionButton(inputId = "tour", label = "Introductory Tour", icon = icon("info-circle")),
                 actionButton(inputId = "close", label = "Close", icon = icon("close"))
             )
         ))
         
         show("landing_page")
         hide("protein_interaction")
         hide("protein_interaction1")
         hide("tabButtons")
         hide("buttonsui")
         #hide("tabButtons2")
         hide("pub")
         hide("cluster_page")
         hide("general_info")
         
     }, once = TRUE)
     
     observeEvent(input$tour, {
         removeModal()
         guide$init()$start()
     })
     
     
     observeEvent(input$close,{
         removeModal()
     })
    
    
     observeEvent(input$geneName_search, { 
          if (genename() == ""){
              sendSweetAlert(
                  session = session,
                  title = "WARNING!",
                  text = "Please first write a gene name, gene id or Ensembl id",
                  type = "warning",
                  showCloseButton = TRUE
              )
              # showModal(modalDialog(
              #     "Please first write a gene name",
              #     easyClose = TRUE,
              #     footer = tagList(
              #         actionButton(inputId = "close", label = "Close", icon = icon("info-circle"))
              #     )
              #     )
              # )
          }
         else if (length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName)])) > 1){
             showModal(modalDialog(
                 h3("Multiple Results"),
                 radioGroupButtons("generadio", h4("It appears there are multiple genes corresponding to the input. Please select one: "), geneoption(), selected = character(0)),
                 easyClose = TRUE,
                 footer = tagList(
                     actionButton(inputId = "close", label = "Close", icon = icon("close"))
                 )
             )
             )
             #click("geneName_reset")
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
             updateProgressBar(session = session, id = "pb8", value = inputseq(), total = 21271)
             show("general_info")
             hide("protein_interaction")
             hide("protein_interaction1")
             hide("landing_page")
             show("buttonsui")
             #hide("tabButtons2")
             hide("cluster_page")
             hide("pub")
             show("back_button")
             click("generalPage")
             #hide("proteinPage")
             updateTabItems(session, "tabs", selected = character(0))
         }
         
     },
     ignoreInit = TRUE,
     ignoreNULL = TRUE
     )
     
     observeEvent(input$generadio, {
         session$sendCustomMessage("geneName", input$generadio)
         #updateProgressBar(session = session, id = "pb8", value = inputseq(), total = 21271)
         show("general_info")
         hide("protein_interaction")
         hide("protein_interaction1")
         hide("landing_page")
         show("buttonsui")
         #hide("tabButtons2")
         hide("cluster_page")
         hide("pub")
         show("back_button")
         click("generalPage")
         #hide("proteinPage")
         updateTabItems(session, "tabs", selected = character(0))
     })
     
    observeEvent(input$geneName, {
        updateProgressBar(session = session, id = "pb8", value = inputseq(), total = 21271)
    })
     
    observeEvent(input$generalPage, {
        show("general_info")
        #hide("tabButtons2")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("pub")
        hide("cluster_page")
    })
    
    observeEvent(input$homePage, {
         show("landing_page")
         hide("general_info")
         hide("protein_interaction")
         hide("protein_interaction1")
         hide("buttonsui")
         #hide("tabButtons2")
         hide("pub")
         hide("cluster_page")
         click("geneName_reset")
         hide("back_button")
         updateTabItems(session, "tabs", "hometab")
    })
    
    observeEvent(input$proteinPage, {
        show("protein_interaction")
        show("protein_interaction1")
        #hide("tabButtons2")
        hide("general_info")
        hide("landing_page")
        hide("pub")
        hide("cluster_page")
        #hide("proteinPage")
    })
    
    observeEvent(input$pubPage, {
        show("pub")
        #hide("tabButtons2")
        hide("general_info")
        hide("landing_page")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("cluster_page")
        #hide("pubPage")
        #show("proteinPage")
    })
    
    observeEvent(input$clusterPage, {
        show("cluster_page")
        #hide("tabButtons2")
        hide("general_info")
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
    
    observeEvent(input$generadio,{
        removeModal()
    })
    
    observeEvent(input$hometab, {
        show("landing_page")
        hide("general_info")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("buttonsui")
        #hide("tabButtons2")
        hide("cluster_page")
        hide("pub")
        hide("back_button")
        
    })
    
    observeEvent(input$tabs == "exploretab", {
        show("exploretab")
        #show("tabButtons2")
        hide("general_info")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("buttonsui")
        hide("pub")
        show("exptab")
        hide("cluster_page")
        hide("back_button")
        #updateTabItems(session, "tabs", "exploretab")
    })
    
    observeEvent(input$tabs == "hometab", {
        show("landing_page")
        hide("general_info")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("buttonsui")
        #hide("tabButtons2")
        hide("pub")
        hide("cluster_page")
        hide("back_button")
        click("geneName_reset")
    })
    
    observeEvent(input$explore, {
        
        updateTabItems(session, "tabs", "exploretab")
        #show("tabButtons2")
    })
    
    observeEvent(genenumber(), {
        if (length(genenumber() != "") != 0){
            if (genenumber() != ""){
                session$sendCustomMessage("geneName", genenumber())
                #updateTabItems(session, "tabs", "hometab")
                hide("exptab")
                #hide("tabButtons2")
                show("buttonsui")
                show("general_info")
                hide("protein_interaction")
                hide("protein_interaction1")
                hide("landing_page")
                show("back_button")
                click("generalPage")
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
                #hide("tabButtons2")
                show("buttonsui")
                show("general_info")
                hide("protein_interaction")
                hide("protein_interaction1")
                hide("landing_page")
                show("back_button")
                click("generalPage")
                updateTabItems(session, "tabs", selected = character(0))
                #click("geneName_search")
            }
        }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE)
    
    
    observeEvent(input$back, {
        hide("general_info")
        hide("protein_interaction")
        hide("protein_interaction1")
        hide("buttonsui")
        hide("pub")
        hide("cluster_page")
        hide("back_button")
        show("exptab")
        #show("tabButtons2")
        show("landing_page")
    })
    
    
    genename<-reactive({
        if (toupper(input$geneName) %in% ens$`Gene stable ID`){
            gname<-ens$`Gene name`[which(ens$`Gene stable ID` %in% toupper(input$geneName))]
        }
        else if (input$geneName %in% homsap$GeneID){
            gname<-homsap$Symbol[which(homsap$GeneID %in% input$geneName)]
        }
            else {
                if (!(toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_name)) && 
                (toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_synonyms)) && 
                length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName)])) == 1) {
                gname<-gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName))]
            }
            else if (toupper(input$geneName) %in% toupper(gene_synonyms2$Gene_name)){
                gname<-unique(gene_synonyms2$Gene_name[which(gene_synonyms2$Gene_name %in% toupper(input$geneName))])
            }
            else {gname<-input$geneName}
            }
        
        gname
    })
    
    geneoption<-reactive({
        
        c(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(input$geneName))])
    })
    
    # generadioname<-reactive({
    #     generadio
    # })

    # Gene info **************** #
    
    geneid<-reactive({
        
        homsap$GeneID[which(homsap$Symbol == genename())]
    })
    
    genedescription<-reactive({
        homsap$description[which(homsap$Symbol == genename())]
    })
    
    genesynonyms<-reactive({
        homsap$Synonyms[which(homsap$Symbol == genename())]
    })
    
    geneensembl<-reactive({
        ens$`Gene stable ID`[which(ens$`Gene name` == genename())]
    })
    
    # *************************** #
    
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
        #zz$`Gold standard`<-"NO"
        #zz$`Gold standard`<-final_score_table1$goldstandard[which(final_score_table1$goldstandard %in% zz[[2]])]
        
        if (length(zz[[1]]) > 0){
            zz$`Gold standard` <- "NO"
            zz$`Gold standard`[which(zz[[2]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
            zz$CilioGenics <- "NO"
            zz$CilioGenics[which(zz[[2]] %in% ciliogenics[[1]])]<-"YES"
        }
        zz[which(zz$Source %in% input$proradio),]
    })
    
    networkdata2<-reactive({
        
        a<-data.frame(rbind(biogrid, intact, wbP))
        colnames(a)<-c("Interactor A", "Interactor B", "Source")
        b<-a[which(a[[1]] %in% networkdata()[[2]]),]
        data.frame(rbind(networkdata(), b))

    })
    
    pubdata<-reactive({
        
        data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))]))
    })
    
    inputcluster<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == aa$cluster_number[which(toupper(aa$Gene_name) == toupper(genename()))]),2:73])
    })
    
    inputclusternamenumber<-reactive({
        
        aa$cluster_number[which(toupper(aa$Gene_name) == toupper(genename()))]
    })
    
    inputclusternumber<-reactive({
        
        as.matrix(nscores2[which(aa$cluster_number == input$clusternumber),2:73])
    })
    
    inputclustertable<-reactive({
        
        df<-data.frame('Gene name' = nscores2[which(aa$cluster_number == aa$cluster_number[which(toupper(aa$Gene_name) == toupper(genename()))]),1])
        df$Score<-final_score_table$Weighted_total_scores[match(df[[1]], final_score_table$Gene_name)]
        df$`Gold standard` <- "NO"
        df$`Gold standard`[which(df[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
        df$CilioGenics <- "NO"
        df$CilioGenics[which(df[[1]] %in% ciliogenics[[1]])]<-"YES"
        df
    })
    
    inputclusternumbertable<-reactive({
        
        a<-data.frame('Gene name' = nscores2[which(aa$cluster_number == input$clusternumber),1])
        a$Score<-final_score_table$Weighted_total_scores[match(a[[1]], final_score_table$Gene_name)]
        a$`Gold standard` <- "NO"
        a$`Gold standard`[which(a[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
        a$CilioGenics <- "NO"
        a$CilioGenics[which(a[[1]] %in% ciliogenics[[1]])]<-"YES"
        a
    })
    
    
    selected <- reactive(getReactableState("generaltable2", "selected"))
    selected2 <- reactive(getReactableState("generaltable3", "selected"))
    
    inputseq<-reactive({
        final_score_table$Seq[which(final_score_table$Gene_name == genename())]
    })
    
    
    
    output$buttonsui<-renderUI({
        if (session$clientData$pixelratio == 1 || session$clientData$pixelratio == 2){
        absolutePanel(
            div(
                id = "tabButtons",
                column(
                    width = 12,
                    align = "center",
                    br(),
                    actionBttn("homePage", "Home",
                               icon = icon("home"), 
                               style = "unite",
                               color = "default",
                               size = "sm"),
                    actionBttn("generalPage", "General Information",
                               icon = icon("info"), 
                               color = "success",
                               style = "unite",
                               size = "sm"),
                    actionBttn("proteinPage", "Protein interactions",
                               icon = img(src = "network2.png", height = "20px"), 
                               color = "success",
                               style = "unite",
                               size = "sm"),
                    
                    actionBttn("clusterPage", "Clusters",
                               icon = img(src = "tree.png", height = "20px"), 
                               color = "success",
                               style = "unite",
                               size = "sm"),
                    
                    actionBttn("pubPage", "Publications",
                               icon = icon("book"), 
                               color = "success",
                               style = "unite",
                               size = "sm"),
                    
                )
            ),
            top = "50px",
            left = 0,
            right = 0,
            fixed = TRUE,
            style = "z-index: 10;"
        )}
        else {
            absolutePanel(
                div(
                    id = "tabButtons",
                    column(
                        width = 12,
                        align = "center",
                        br(),
                        actionBttn("homePage", "Home",
                                   icon = icon("home"), 
                                   style = "unite",
                                   color = "default",
                                   size = "sm"),
                        actionBttn("generalPage", "General Information",
                                   icon = icon("info"), 
                                   color = "success",
                                   style = "unite",
                                   size = "sm"),
                        actionBttn("proteinPage", "Protein interactions",
                                   icon = img(src = "network2.png", height = "20px"), 
                                   color = "success",
                                   style = "unite",
                                   size = "sm"),
                        
                        actionBttn("clusterPage", "Clusters",
                                   icon = img(src = "tree.png", height = "20px"), 
                                   color = "success",
                                   style = "unite",
                                   size = "sm"),
                        
                        actionBttn("pubPage", "Publications",
                                   icon = icon("book"), 
                                   color = "success",
                                   style = "unite",
                                   size = "sm"),
                        
                    )
                ),
                top = "35px",
                left = 0,
                right = 0,
                fixed = FALSE,
                style = "z-index: 10;"
            ) 
        }
    })
    
    
    # output$modalgene<-renderUI({
    #     option1<-geneoption()
    #     radioButtons("generadio", "Which genes", geneoption(), selected = character(0))
    # })
    
    
    output$networkplot<-renderSimpleNetwork({
        simpleNetwork(networkdata(), height = "200px", width = "200px", zoom = TRUE,
                      opacity = 1, fontSize = 8)
    })
    
    output$networkplot2<-renderSimpleNetwork({
        simpleNetwork(networkdata2(), height = "200px", width = "200px", zoom = TRUE,
                      opacity = 1, fontSize = 8, charge = -10)
    })
    
    output$pro_int<-renderReactable({
        
        reactable(networkdata(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
    })
    
    output$pro_int2<-renderReactable({
        
        reactable(networkdata2(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
    })
    
    output$textgeneid<-renderText({
        
        paste(
            "<table style=\"font-size:17px\">", 
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Gene name:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", genename(), "</td>", "</tr>",
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Gene description:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", genedescription(), "</td>", "</tr>",
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "NCBI gene ID:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "</b>", geneid(), "</td>", "</tr>",

            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Synonyms:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "</b>", genesynonyms(), "</td>", "</tr>",
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Ensembl ID:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", geneensembl(), "</td>", "</tr>",
            
            "<table>")
    })
    
    
    output$protable<-renderUI({
        boxPlus(
            width = 12,
            title = paste("Genes interacting with ", genename(), "gene"),
            solidHeader = TRUE,
            status = "success",
            # if (input$pro_children == TRUE){
            #     
            #     withSpinner(reactableOutput("pro_int2"), color = "#10c891")
            # }
            withSpinner(reactableOutput("pro_int"), color = "#10c891")
        )
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
                  # defaultColDef = colDef(
                  #     style = function(value) {
                  #         if (!is.numeric(value)) return()
                  #         normalized <- (value - min(final_score_table[,-1])) / (max(final_score_table[,-1]) - min(final_score_table[,-1]))
                  #         color <- GnYlRd(normalized)
                  #         list(background = color)
                  #     },
                  #     format = colFormat(digits = 1),
                  #     minWidth = 50
                  # ),
                  rowStyle = list(cursor = "pointer"),
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
                  rowStyle = list(cursor = "pointer"),
                  selection = "single",
                  onClick = "select"
        )
        
    })
    
    output$textForPro<-renderText({paste("There is no protein interaction for", genename(), "gene")})
    #output$ot<-renderText({paste("this is", genename())})
    

    
        output$pro_box1 <- renderUI({
            
                boxPlus(
                    title = paste0("Protein interaction network for ", genename(), " gene"),
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    if (length(networkdata()[[1]]) == 0){
                        #print(paste("There is no protein interaction for", genename(), "gene"))
                        textOutput("textForPro")
                    }
                    else {#if (input$pro_children == FALSE){
                        
                        withSpinner(simpleNetworkOutput("networkplot"), color = "#10c891")
                    }
                    # else if (length(networkdata2()[[1]] != 0)){
                    #     
                    #     withSpinner(simpleNetworkOutput("networkplot2"), color = "#10c891")
                    # }
                )
        })
        
        output$clusterui<-renderUI({
            
            column(
                width = 9,
                boxPlus(
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    title = paste("Cluster", inputclusternamenumber()),
                    withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "600px"), color = "#10c891")))
        })
        
        output$clustertableui<-renderUI({
            
            
            column(
                width = 3,
                boxPlus(
                    title = paste("Genes in cluster", inputclusternamenumber()),
                    solidHeader = TRUE,
                    status = "success",
                    width = 12,
                    withSpinner(reactableOutput("hclustertable"), color = "#10c891")
                )
                
            )
        })
    
    output$pubtable<-renderReactable({
        reactable(pubdata(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
                  highlight = TRUE)
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
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
                  highlight = TRUE,
                  columns = list(
                      Score = colDef(name = "Score", 
                             format = colFormat(digits = 3)),
                      gene_name = colDef(name = "Gene name")
                  ))
    })
    
    output$hclusternumbertable<-renderReactable({
        
        reactable(inputclusternumbertable(), resizable = TRUE, filterable = TRUE,
                  columns = list(Score = colDef(format = colFormat(digits = 3)),
                                 gene_name = colDef(name = "Gene name")),
                  defaultPageSize = 10, showPageSizeOptions = TRUE,
                  highlight = TRUE)
    })
    
    output$heatmapclusternumber<-renderIheatmap({
        
        main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'))%>%
            add_row_labels(size = 0.05,font = list(size = 7))%>%
            add_col_labels(size = 0.6,font = list(family = "Open Sans", size = 14), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })

    
}
