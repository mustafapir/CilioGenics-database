
library(shiny)
library(webshot)
webshot::install_phantomjs
source("functions.R")
options(reactable.theme = reactableTheme(
    #backgroundColor = "CadetBlue",
    #borderColor = "SteelBlue",
    
))

GnYlRd <- function(x) rgb(colorRamp(c("#63be7b", "#ffeb84", "#f87274"))(x), maxColorValue = 255)

server <- function(input, output, session) { 
    
    js$getcookie()
     observeEvent("", {
         
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
     
    observeEvent(input$jscookie,{
        if (!is.null(input$jscookie) && input$jscookie != "" && input$jscookie == sessionid) {
            
        }
        else {
            showModal(modalDialog(
                includeHTML("intro.html"),
                easyClose = TRUE,
                footer = tagList(
                    actionButton(inputId = "tour", label = "Introductory Tour", icon = icon("info-circle")),
                    actionButton(inputId = "close", label = "Close", icon = icon("close"))
                )))
        }
    })
    
     observeEvent(input$tour, {
         removeModal()
         guide$init()$start()
         js$setcookie(sessionid)
         #introjs(session, options = list("nextLabel"="Next", "prevLabel" = "Prev"))
     })
     
     
     observeEvent(input$close,{
         removeModal()
         js$setcookie(sessionid)
     })
    
     
    
     observeEvent(input$geneName_search, { 
         session$sendCustomMessage("geneName", trimmedGname())
          if (genename() == ""){
              sendSweetAlert(
                  session = session,
                  title = "WARNING!",
                  text = "Please first write a gene name, gene id or Ensembl id",
                  type = "warning",
                  showCloseButton = TRUE
              )
          }
         else if (length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname())])) > 1){
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
             #guide1$init()$start()
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
     
    observeEvent(trimmedGname(), {
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
    
    
     observeEvent(selected3(), {
         if (length(genenumbercluster() != "") != 0){
             if (genenumbercluster() != ""){
                 session$sendCustomMessage("geneName", genenumbercluster())
                 #updateTabItems(session, "tabs", "hometab")
                 #hide("exptab")
                 #hide("tabButtons2")
                 #show("buttonsui")
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
    
     
    observeEvent(genenumbergeneralcluster(), {
         if (length(genenumbergeneralcluster() != "") != 0){
             if (genenumbergeneralcluster() != ""){
                 session$sendCustomMessage("geneName", genenumbergeneralcluster())
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
    
    observeEvent(input$help1, {
        #guide1$init()$start()
    })
    
    observeEvent(inputclustergname(), {
        toggleDropdownButton(inputId = "drpdwnbttn")
    }, ignoreInit = TRUE)
    
    trimmedGname<-reactive({
        trimmed<-trimws(input$geneName)
        trimmed
    })
    
    genename<-reactive({
        if (toupper(trimmedGname()) %in% ens$`Gene stable ID`){
            gname<-ens$`Gene name`[which(ens$`Gene stable ID` %in% toupper(trimmedGname()))]
        }
        else if (trimmedGname() %in% homsap$GeneID){
            gname<-homsap$Symbol[which(homsap$GeneID %in% trimmedGname())]
        }
            else {
                if (!(toupper(trimmedGname()) %in% toupper(gene_synonyms2$Gene_name)) && 
                (toupper(trimmedGname()) %in% toupper(gene_synonyms2$Gene_synonyms)) && 
                length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname())])) == 1) {
                gname<-gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname()))]
            }
            else if (toupper(trimmedGname()) %in% toupper(gene_synonyms2$Gene_name)){
                gname<-unique(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_name) %in% toupper(trimmedGname()))])
            }
            else {gname<-trimmedGname()}
            }
        
        gname
    })
    
    geneoption<-reactive({
        
        c(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname()))])
    })

    # Gene info **************** #
    
    geneid<-reactive({
        
        homsap$GeneID[which(homsap$Symbol == genename())]
    })
    
    genedescription<-reactive({
        homsap$description[which(homsap$Symbol == genename())]
    })
    
    genesynonyms<-reactive({
        a<-homsap2$Synonyms[which(homsap2$Symbol == genename())]
        if (length(a) > 1){
            a<-paste0(a, collapse = "|\n")
        }
        a
    })
    
    geneensembl<-reactive({
        a<-ens$`Gene stable ID`[which(ens$`Gene name` == genename())]
        if (length(a) > 1){
            a<-paste0(a, collapse = "|\n")
        }
        a
    })
    
    # *************************** #
    
    genenumber<-reactive({
        final_score_table$Gene_name[selected()]
    })
    
    genenumber2<-reactive({
        final_seq_table$Gene_name[selected2()]
    })
    
    genenumbercluster<-reactive({
        inputclustertable()[[1]][selected3()]
    })
    
    genenumbergeneralcluster<-reactive({
        inputclusternumbertable()[[1]][selected4()]
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
        
        a<-data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))]))
        b<-merge(a, publ, by = "Publication")
        b$Link <- paste0("<a href='",b$Link,"' target='_blank'>",b$Link,"</a>")
        b
    })
    
    inputcluster<-reactive({
        
        a<-as.matrix(nscores2[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]),2:73])
        if (length(a[,1]) > 500){
            a<-a[1:500,]
        }
        rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))])]
        a
    })
    
    inputclusterGene<-reactive({
        
        a<-as.matrix(nscores2[which(nscores2$Gene_name == toupper(genename())),2:73])
        a<-rbind(a,a)
        rownames(a)<-c(genename(),"")
        a
    })
    
    inputclusternamenumber<-reactive({
        
        nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]
    })
    
    inputclusternumber<-reactive({
        a<-as.matrix(nscores2[which(nscores2$cluster_number == input$clusternumber),2:73])
        rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == input$clusternumber)]
        a
    })
    
    inputclustertable<-reactive({
        
        df<-data.frame('Gene name' = nscores2[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]),1], stringsAsFactors = FALSE)
        df$Score<-final_score_table$Weighted_total_scores[match(df[[1]], final_score_table$Gene_name)]
        df$`Gold standard` <- "NO"
        df$`Gold standard`[which(df[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
        df$CilioGenics <- "NO"
        df$CilioGenics[which(df[[1]] %in% ciliogenics[[1]])]<-"YES"
        df
    })
    
    inputclusternumbertable<-reactive({
        
        a<-data.frame('Gene name' = nscores2[which(nscores2$cluster_number == input$clusternumber),1])
        a$Score<-final_score_table$Weighted_total_scores[match(a[[1]], final_score_table$Gene_name)]
        a$`Gold standard` <- "NO"
        a$`Gold standard`[which(a[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
        a$CilioGenics <- "NO"
        a$CilioGenics[which(a[[1]] %in% ciliogenics[[1]])]<-"YES"
        a
    })
    
    
    selected <- reactive(getReactableState("generaltable2", "selected"))
    selected2 <- reactive(getReactableState("generaltable3", "selected"))
    selected3 <- reactive(getReactableState("hclustertable", "selected"))
    selected4 <- reactive(getReactableState("hclusternumbertable", "selected"))
    
    inputscore<-reactive({
        
        if (length(final_score_table$Weighted_total_scores[which(final_score_table$Gene_name == genename())]) == 0){
            paste("There is no score for",genename(), "gene.")
        }
        else {as.numeric(format(round(final_score_table$Weighted_total_scores[which(final_score_table$Gene_name == genename())], 3), nsmall = 3))}
    })
    
    
    # inputscorestate<-reactive({
    #     if (inputscore() >= 0.5){
    #         "<H4 style=\"color:#009a00\";>High probability</H4>"
    #     }
    #     else if (inputscore() < 0.5 && inputscore() >= 0.4){
    #         "<H4 style=\"color:#38a1db\";>Mild probability</H4>"
    #     }
    #     else {"<H4 style=\"color:#f01a1a\";>Low probability</H4>"}
    # })
    
    inputscorestate<-reactive({
        if (!is.character(inputscore())){
            if (inputscore() >= 0.5){
                dashboardLabel("High Probability", status = "success")
            }
            else if (inputscore() < 0.5 && inputscore() >= 0.4){
                dashboardLabel("Mild Probability", status = "info")
            }
            else {dashboardLabel("Low Probability", status = "danger")}
        }
        else {dashboardLabel("Low Probability", status = "danger")}
    })
    
    
    inputseq<-reactive({
        final_score_table$Seq[which(final_score_table$Gene_name == genename())]
    })
    
    inputseq1<-reactive({
        final_seq_table$Protein_interaction_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq2<-reactive({
        final_seq_table$Genetic_interaction_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq3<-reactive({
        final_seq_table$Single_cell_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq4<-reactive({
        final_seq_table$Cluster_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq5<-reactive({
        final_seq_table$Motif_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq6<-reactive({
        final_seq_table$Publication_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputseq7<-reactive({
        final_seq_table$Protein_atlas_score[which(final_seq_table$Gene_name == genename())]
    })
    
    inputclustergname<-reactive({
        input$hmapradio
    })
    
    
    reactiveHeatmap1<-reactive({
        hmap <- isolate(inputcluster())
        main_heatmap(hmap, layout = list(paper_bgcolor='transparent'), 
                               tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
            add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 9))%>%
            add_col_labels(size = 0.46, font = list(family = c("open_sansregular"), size = 12), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })
    
    reactiveHeatmap2<-reactive({
        hmap <- isolate(inputclusterGene())
        main_heatmap(hmap, layout = list(paper_bgcolor='transparent'), 
                     tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
            add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 12))%>%
            add_col_labels(size = 1, font = list(family = c("open_sansregular"), size = 12), textangle=90, 
                           tickvals = c(1:length(colnames(inputcluster()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
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
                               size = "sm")
                )
            ),
            top = "50px",
            left = 0,
            right = 0,
            fixed = TRUE,
            style = "z-index: 10;"
        )
        }
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
                                   size = "sm")
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
    
    output$space<-renderUI({
        if (session$clientData$pixelratio == 1 || session$clientData$pixelratio == 2){
            
        }
        else {br()
            br()
            br()}
    })
    
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
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "CilioGenics score:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", inputscore(), "</td>", "</tr>",
            
            "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Probability of being ciliary gene:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", inputscorestate(), "</td>", "</tr>",
            
            "<table>")
    })
    
    
     output$bargeneinfo<-renderUI({
         
         genebar("pb", inputseq(), "Overall percentile", 1165, 1930)
     })
     output$bargeneinfo1<-renderUI({    
         genebar("pb1", inputseq1(), "Protein interaction", 2194, 8438)
     })
     output$bargeneinfo2<-renderUI({    
         genebar("pb2", inputseq2(), "Genetic interaction", 285, 317)
     })
     output$bargeneinfo3<-renderUI({    
         genebar("pb3", inputseq3(), "Single cell", 1868, 2923)
     })
     output$bargeneinfo4<-renderUI({    
         genebar("pb4", inputseq4(), "Cluster", 4845, 7759)
     })
     output$bargeneinfo5<-renderUI({    
         genebar("pb5", inputseq5(), "Motif", 702, 1878)
     })
     output$bargeneinfo6<-renderUI({    
         genebar("pb6", inputseq6(), "Publication", 7302, 11690)
     })
     output$bargeneinfo7<-renderUI({    
         genebar("pb7", inputseq7(), "Protein Atlas", 651, 1016)
     
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
                        textOutput("textForPro")
                    }
                    else {
                        withSpinner(simpleNetworkOutput("networkplot"), color = "#10c891")
                    }
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
                    height = "800px",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "hmapradio",
                                label = "Genes to display:", 
                                choiceNames = c("Cluster", genename()), 
                                choiceValues = c("cluster", "gene"), 
                                selected = "year", 
                                direction = "vertical"
                            ),
                            size = "xm",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    
                    if (length(inputclustergname()) == 0){
                        downloadFunction("hmap1")
                    }
                    else {
                        if (inputclustergname() == "cluster"){
                            downloadFunction("hmap1")
                        }
                        else{downloadFunction("hmap2")}
                    }
                    ,
                    if (length(inputclustergname()) == 0){
                        withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "700px"), color = "#10c891")
                    }
                    else {
                        if (inputclustergname() == "cluster"){
                            withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "700px"), color = "#10c891")
                        }
                        else{withSpinner(iheatmaprOutput("heatmapclusterGene", width = "100%", height = "700px"), color = "#10c891")}
                    }
                )
            )
        })
        
        output$clstrui1<-renderUI({
            if (inputclustergname() == "Cluster"){
                withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "600px"), color = "#10c891")
            }
            else{withSpinner(iheatmaprOutput("heatmapclusterGene", width = "100%", height = "420px"), color = "#10c891")}
        })
        
        output$textclstr<-renderUI({
            titleTag <- shiny::tags$h3(class = "box-title", paste("Cluster", inputclusternamenumber()))
            headerTag <- shiny::tags$div(class = "box-header", titleTag)
            boxClass <- paste("box", "box-solid")
            boxClass <- paste0(boxClass, " box-", "success")
            shiny::tags$div(class = boxClass, headerTag)
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
                  highlight = TRUE,
                  columns = list(
                      Link = colDef(html = TRUE)
                  )
        )
        })
    
    output$puberror<-renderText({paste("There is no publication data for", genename(), "gene")})
    
    output$pubui<-renderUI({
        if (length(unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))])) > 0){
            reactableOutput("pubtable")
        }
        else {h4(textOutput("puberror"))}
    })

    
    output$heatmapcluster<-renderIheatmap({
        input$clusterPage
        reactiveHeatmap1()
    })
    
    output$heatmapclusterGene<-renderIheatmap({
        input$clusterPage
        reactiveHeatmap2()
    })
    
    output$hclustertable<-renderReactable({
        
        reactable(inputclustertable(), resizable = TRUE, filterable = TRUE,
                  searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
                  highlight = TRUE,
                  columns = list(
                      Score = colDef(name = "Score", 
                             format = colFormat(digits = 3)),
                      Gene_name = colDef(name = "Gene name")
                  ),
                  rowStyle = list(cursor = "pointer"),
                  selection = "single",
                  onClick = "select")
    })
    
    output$hclusternumbertable<-renderReactable({
        
        reactable(inputclusternumbertable(), resizable = TRUE, filterable = TRUE,
                  columns = list(Score = colDef(format = colFormat(digits = 3)),
                                 Gene_name = colDef(name = "Gene name")),
                  defaultPageSize = 10, showPageSizeOptions = TRUE,
                  highlight = TRUE,
                  rowStyle = list(cursor = "pointer"),
                  selection = "single",
                  onClick = "select")
    })
    
    output$heatmapclusternumber<-renderIheatmap({
        
        main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'),
                     tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
            add_row_labels(size = 0.03,font = list(family = c("open_sansregular"), size = 7))%>%
            add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90, 
                           tickvals = c(1:length(colnames(inputclusternumber()))))%>%
            add_col_annotation(annotation=anot, side="top", size = 0.1)
    })
    
    
    output$hmap1<-downloadHandler(
        filename =paste0("cluster_", inputclusternamenumber(), ".png"),
        content = function(file){
            save_iheatmap(reactiveHeatmap1(), file, vwidth=2000,vheight=1000)
        },
        contentType = "image/png"
    )
    
    output$hmap2<-downloadHandler(
        filename =paste0("cluster_", genename(), ".png"),
        content = function(file){
            save_iheatmap(reactiveHeatmap2(), file, vwidth=2000,vheight=1000)
        },
        contentType = "image/png"
    )
    
}
