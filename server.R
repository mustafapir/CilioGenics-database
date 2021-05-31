
library(shiny)
#library(webshot)

source("functions.R")

# Server ----
server <- function(input, output, session){
  js$getcookie()
  
  # Dark theme
  observeEvent(input$darkmode,{
    if (input$darkmode == TRUE){
      output$dark<-renderUI({
        includeCSS("www/dark_mode.css")
      })
    }
    else{
      output$dark<-renderUI({
        
      })
    }
  })
  
  customnumber<-reactiveVal(0)
  # Show modal
  observeEvent(input$jscookie,{
    if (!is.null(input$jscookie) && input$jscookie != "" && input$jscookie == sessionid) {
     
    }
    else {
      showModal(
        modalDialog(
          includeHTML("intro.html"),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = "tour", label = "Introductory Tour", icon = icon("info-circle")),
            actionButton(inputId = "close", label = "Close", icon = icon("close"))
            )
          )
        )
      output$cookie_footer<-renderUI({
        tags$footer(cookie_box)
      })
      }
    })

  # Show tour guide
  observeEvent(input$tour, {
    removeModal()
    guide$init()$start()
    js$setcookie(sessionid)
    nn<-customnumber()+1
    customnumber(nn)
  })

  # Remove modal
  observeEvent(input$close,{
    removeModal()
    js$setcookie(sessionid)
  })

  observeEvent(input$generadio,{
    removeModal()
  })

  ## Gene name operations ----
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

  # List of genes having same synonym gene names
  geneoption<-reactive({
    c(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname()))])
  })

  # Show only landing page
  observeEvent("", {
    #show("landing_page")
    # hide("protein_interaction")
    # hide("protein_interaction1")
    # hide("tabButtons")
    # hide("buttonsui")
    # hide("pub")
    # hide("single_cell")
    # hide("cluster_page")
    # hide("sc_cluster_page")
    # hide("general_info")
    # hide("searchui")
    # hide("searchUI")
    hide("nvbr")
    hide("inpt")
    if (!input$isMobile){
      js$hidehead('none')
    }
  }, once = TRUE)

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
      showModal(
        modalDialog(
          h3("Multiple Results"),
          radioGroupButtons(
            "generadio",
            h4("It appears there are multiple genes corresponding to the input. Please select one: "),
            geneoption(),
            selected = character(0)
            ),
          easyClose = TRUE,
          footer = tagList(
            actionButton(
              inputId = "close",
              label = "Close",
              icon = icon("close")
              )
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
    }

    else {
      # show("general_info")
      # hide("protein_interaction")
      # hide("protein_interaction1")
      # hide("landing_page")
      # hide("toggleui2")
      # show("buttonsui")
      # hide("cluster_page")
      # hide("sc_cluster_page")
      # hide("pub")
      # hide("single_cell")
      # show("back_button")
      # click("generalPage")
      # show("searchui")
      # show("searchUI")
      show("nvbr")
      show("inpt")
      updateNavbarPage(inputId = "nvbr", selected = "General info")
      if(customnumber() == 1){
        showModal(
          modalDialog(
            "Continue the tour?",
            easyClose = TRUE,
            footer = tagList(
              actionButton(inputId = "tour2", label = "Yes", icon = icon("info-circle")),
              actionButton(inputId = "close2", label = "Close", icon = icon("close"))
            )
          )
        )
      }
      
      #updateTabItems(session, "tabs", selected = character(0))
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
    )
  
  observeEvent(input$tour2, {
    removeModal()
    guide2$init()$start()
  })
  
  # Remove modal
  observeEvent(input$close2,{
    removeModal()
  })
  
  # observeEvent(input$geneName2_search, {
  #   session$sendCustomMessage("geneName", input$geneName2)
  # })
  
  observeEvent(input$geneName2_search, {
    if (input$geneName2 == ""){
      # sendSweetAlert(
      #   session = session,
      #   title = "WARNING!",
      #   text = "Please first write a gene name, gene id or Ensembl id",
      #   type = "warning",
      #   showCloseButton = TRUE
      # )
    }
    else if (length(unique(gene_synonyms2$Gene_name[toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname())])) > 1){
      showModal(
        modalDialog(
          h3("Multiple Results"),
          radioGroupButtons(
            "generadio",
            h4("It appears there are multiple genes corresponding to the input. Please select one: "),
            geneoption(),
            selected = character(0)
          ),
          easyClose = TRUE,
          footer = tagList(
            actionButton(
              inputId = "close",
              label = "Close",
              icon = icon("close")
            )
          )
        )
      )
    }
    else if (!(toupper(input$geneName2) %in% toupper(gene_synonyms2$Gene_name))){
      sendSweetAlert(
        session = session,
        title = "ERROR!",
        text = "Please check the gene name and try again",
        type = "error",
        showCloseButton = TRUE
      )
    }
    else {
      session$sendCustomMessage("geneName", input$geneName2)
    }
  })

  observeEvent(input$generadio, {
    session$sendCustomMessage("geneName", input$generadio)
    click("generalPage")
    show("general_info")
    hide("protein_interaction")
    hide("protein_interaction1")
    hide("landing_page")
    hide("toggleui2")
    show("buttonsui")
    hide("cluster_page")
    hide("sc_cluster_page")
    hide("pub")
    hide("single_cell")
    show("searchui")
    show("searchUI")
    click("generalPage")
    updateTabItems(session, "tabs", selected = character(0))
  })

  observeEvent(input$ARL13B, {
    session$sendCustomMessage("geneName", "ARL13B")
    # click("generalPage")
    # show("general_info")
    # hide("protein_interaction")
    # hide("protein_interaction1")
    # hide("landing_page")
    # hide("toggleui2")
    # show("buttonsui")
    # hide("cluster_page")
    # hide("sc_cluster_page")
    # hide("pub")
    # hide("single_cell")
    # show("searchui")
    # show("searchUI")
    # click("generalPage")
    # updateTabItems(session, "tabs", selected = character(0))
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })

  observeEvent(input$IFT74, {
    session$sendCustomMessage("geneName", "ENSG00000096872")
    # click("generalPage")
    # show("general_info")
    # hide("protein_interaction")
    # hide("protein_interaction1")
    # hide("landing_page")
    # hide("toggleui2")
    # show("buttonsui")
    # hide("cluster_page")
    # hide("sc_cluster_page")
    # hide("pub")
    # hide("single_cell")
    # show("searchui")
    # show("searchUI")
    # click("generalPage")
    # updateTabItems(session, "tabs", selected = character(0))
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })

  observeEvent(input$BBS5, {
    session$sendCustomMessage("geneName", "129880")
    # click("generalPage")
    # show("general_info")
    # hide("protein_interaction")
    # hide("protein_interaction1")
    # hide("landing_page")
    # hide("toggleui2")
    # show("buttonsui")
    # hide("cluster_page")
    # hide("sc_cluster_page")
    # hide("pub")
    # hide("single_cell")
    # show("searchui")
    # show("searchUI")
    # click("generalPage")
    # updateTabItems(session, "tabs", selected = character(0))
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })

  # observeEvent(trimmedGname(), {
  #   updateProgressBar(session = session, id = "pb8", value = inputseq(), total = 21271)
  # })

  # observeEvent(input$homePage, {
  #   show("landing_page")
  #   show("toggleui2")
  #   hide("general_info")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("buttonsui")
  #   hide("pub")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  #   hide("single_cell")
  #   click("geneName_reset")
  #   click("geneName2_reset")
  #   hide("searchui")
  #   hide("searchUI")
  #   updateTabItems(session, "tabs", "hometab")
  # })
  # 
  # observeEvent(input$generalPage, {
  #   show("general_info")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("pub")
  #   hide("single_cell")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  # })
  # 
  # observeEvent(input$proteinPage, {
  #   show("protein_interaction")
  #   show("protein_interaction1")
  #   hide("general_info")
  #   hide("landing_page")
  #   hide("pub")
  #   hide("single_cell")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  # })
  # 
  # observeEvent(input$clusterPage, {
  #   show("cluster_page")
  #   hide("general_info")
  #   hide("landing_page")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("pub")
  #   hide("sc_cluster_page")
  #   hide("single_cell")
  # })
  # 
  # observeEvent(input$scclusterPage, {
  #   show("sc_cluster_page")
  #   hide("general_info")
  #   hide("landing_page")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("pub")
  #   hide("cluster_page")
  #   hide("single_cell")
  # })
  # 
  # observeEvent(input$pubPage, {
  #   show("pub")
  #   hide("general_info")
  #   hide("landing_page")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("single_cell")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  # })

  # observeEvent(input$hometab, {
  #   show("landing_page")
  #   hide("general_info")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("buttonsui")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  #   hide("pub")
  #   hide("single_cell")
  #   hide("back_button")
  # 
  # })

  # observeEvent(input$tabs == "exploretab", {
  #   show("exploretab")
  #   hide("general_info")
  #   hide("protein_interaction")
  #   hide("protein_interaction1")
  #   hide("buttonsui")
  #   hide("pub")
  #   hide("single_cell")
  #   show("exptab")
  #   hide("cluster_page")
  #   hide("sc_cluster_page")
  #   hide("back_button")
  #   updateReactable("hclusternumbertable", selected = NA)
  # })

  # observeEvent(input$tabs == "hometab", {
  #   show("hometab")
  #   # show("landing_page")
  #   # hide("general_info")
  #   # hide("protein_interaction")
  #   # hide("protein_interaction1")
  #   # hide("buttonsui")
  #   # hide("pub")
  #   # hide("single_cell")
  #   # hide("cluster_page")
  #   # hide("sc_cluster_page")
  #   # hide("back_button")
  #   #click("geneName_reset")
  #   updateReactable("hclusternumbertable", selected = NA)
  # })
  
  # eventt<-eventReactive(input$nvbr == "Home", {
  #   runif(1)
  # })
  # observeEvent(eventt(), {
  #   hide("nvbr")
  # })
  
  observeEvent(input$nvbr, {
    if (input$nvbr == "Home"){
      hide("nvbr")
      hide("inpt")
      #hide("geneName2")
    }
  })

  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "exploretab")
  })

  ## Gene select from table ----
  # Behaviour when selecting gene from table
  observeEvent(genenumber(), {
    if (length(genenumber() != "") != 0){
      if (genenumber() != ""){
        session$sendCustomMessage("geneName", genenumber())
        updateTabItems(session, "tabs", selected = "hometab")
        updateNavbarPage(inputId = "nvbr", selected = "General info")
        # hide("exptab")
        # show("buttonsui")
        # show("general_info")
        # hide("protein_interaction")
        # hide("protein_interaction1")
        # hide("landing_page")
        # click("generalPage")
        # show("searchui")
        # show("searchUI")
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE)

  genenumber2<-reactive({
    final_seq_table$Gene_name[selected2()]
  })

  observeEvent(genenumber2(), {
    if (length(genenumber2() != "") != 0){
      if (genenumber2() != ""){
        session$sendCustomMessage("geneName", genenumber2())
        updateNavbarPage(inputId = "nvbr", selected = "General info")
        # hide("exptab")
        # show("buttonsui")
        # show("general_info")
        # hide("protein_interaction")
        # hide("protein_interaction1")
        # hide("landing_page")
        # show("searchui")
        # show("searchUI")
        # click("generalPage")
        # updateTabItems(session, "tabs", selected = character(0))
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE)

  genenumbercluster<-reactive({
    inputclustertable()[[1]][selected3()]
  })

  observeEvent(selected3(), {
    if (length(genenumbercluster() != "") != 0){
      if (genenumbercluster() != ""){
        session$sendCustomMessage("geneName", genenumbercluster())
        updateNavbarPage(inputId = "nvbr", selected = "General info")
        # show("general_info")
        # hide("protein_interaction")
        # hide("protein_interaction1")
        # hide("landing_page")
        # show("back_button")
        # click("generalPage")
        # updateTabItems(session, "tabs", selected = character(0))
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE)

  genenumbergeneralcluster<-reactive({
    inputclusternumbertable()[[1]][selected4()]
  })

  observeEvent(genenumbergeneralcluster(), {
    if (length(genenumbergeneralcluster() != "") != 0){
      if (genenumbergeneralcluster() != ""){
        session$sendCustomMessage("geneName", genenumbergeneralcluster())
        updateNavbarPage(inputId = "nvbr", selected = "General info")
        # hide("exptab")
        # show("buttonsui")
        # show("general_info")
        # hide("protein_interaction")
        # hide("protein_interaction1")
        # hide("landing_page")
        # show("searchui")
        # show("searchUI")
        # click("generalPage")
        # updateTabItems(session, "tabs", selected = character(0))
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE)


  # Modal for phylogeny page ----
  observeEvent(input$clusterPage, {
    req(input$clusterPage, inputclusternamenumber())
    if (length(inputcluster()[,1]) == 500){
      showModal(
        modalDialog(
        h3("Multiple Results"),
        h4("There are more than 500 genes in this cluster. Only first 500 genes are shown on the heatmap. To see the full map, refer to \"Clusters\" tab of gene list page"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "open", label = "I'm lazy. You take me there", icon = icon("star")),
          actionButton(inputId = "close", label = "Close", icon = icon("close"))
        )
        )
      )
    }
  })

  observeEvent(input$open, {
    updateTabItems(session, "tabs", "exploretab")
    updateTabsetPanel(session, "exploredt", selected = "tab2")
    updatePickerInput(session, "clusternumber", selected = inputclusternamenumber())
    removeModal()
    js$setcookie(sessionid)
  })

  # df5<-eventReactive(input$scPage, {
  #   df4
  # })

  # observeEvent(input$hmapradio, {
  #   session$sendCustomMessage("close_drop1", "")
  # })
  #
  # observeEvent(input$hmap1, {
  #   session$sendCustomMessage("close_drop1", "")
  # })
  #
  # observeEvent(input$colcolcol, {
  #   session$sendCustomMessage("close_drop2", "")
  # })

  # General tab ----

  output$searchUI<-renderUI({
    if (!input$isMobile){
      div(
        id = "searchui",
        column(
          width = 1,
          style = "background-color: #00bcd4; border-radius: 15px 0 0 15px;",
          actionButton("toggleSidebar", icon("th"), style = "padding-top: 20px; padding-bottom: 12px;")
        ),
        column(
          width = 11,
          offset = -1,
          tags$script(src = "enter_button2.js"),
          align = "center",
          style = "background-color: #00bcd4; border-radius: 0 15px 15px 0;",#6f7dc8
          br(),
          searchInput(
            inputId = "geneName2",
            #label = HTML("<h3><center>Gene Search</center></h3>"),
            placeholder = "Search genes by gene name, gene id or Ensembl gene id",
            btnSearch = icon("search"),
            btnReset = icon("remove"),
            width = "400px",
            value = NULL
          )
        )
      )
    }
    else {}
  })
  
  ### Menu ----
  
  output$buttonsui<-renderUI({
    if (!input$isMobile){
      absolutePanel(
        div(
          id = "tabButtons",
          column(
            width = 12,
            align = "center",
            offset = 1,
            br(),
            actionBttn("homePage", "Home",
                       icon = icon("home"),
                       style = "fill",
                       color = "default",
                       size = "sm"),
            actionBttn("generalPage", "General Information",
                       icon = icon("info"),
                       color = "primary",
                       style = "fill",
                       size = "sm"),
            actionBttn("proteinPage", "Protein interactions",
                       icon = img(src = "network2.png", height = "20px"),
                       color = "primary",
                       style = "fill",
                       size = "sm"),
            actionBttn("scclusterPage", "Single Cell Clusters",
                       icon = img(src = "tree.png", height = "20px"),
                       color = "primary",
                       style = "fill",
                       size = "sm"),
            br(),
            actionBttn("clusterPage", "Phylogenetic Analysis",
                       icon = img(src = "tree.png", height = "20px"),
                       color = "primary",
                       style = "fill",
                       size = "sm"),
            actionBttn("pubPage", "Publications",
                       icon = icon("book"),
                       color = "primary",
                       style = "fill",
                       size = "sm")
          )
        ),
        top = "70px",
        left = 0,
        right = 0,
        fixed = FALSE,
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
            br(),br(),
            actionBttn("homePage", "Home",
                       icon = icon("home"),
                       style = "unite",
                       color = "default",
                       size = "sm"),
            actionBttn("generalPage", "General Information",
                       icon = icon("info"),
                       color = "primary",
                       style = "unite",
                       size = "sm"),
            actionBttn("proteinPage", "Protein interactions",
                       icon = img(src = "network2.png", height = "20px"),
                       color = "primary",
                       style = "unite",
                       size = "sm"),
            actionBttn("scclusterPage", "Single Cell Clusters",
                       icon = img(src = "tree.png", height = "20px"),
                       color = "primary",
                       style = "unite",
                       size = "sm"),
            actionBttn("clusterPage", "Phylogenetic Analysis",
                       icon = img(src = "tree.png", height = "20px"),
                       color = "primary",
                       style = "unite",
                       size = "sm"),
            actionBttn("pubPage", "Publications",
                       icon = icon("book"),
                       color = "primary",
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
    if (!input$isMobile){

    }
    else {
      br()
      br()
      br()
    }
  })

  ### General page ----
  #### Gene info ----
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

  omim_link<-reactive({
    a<-as.character(omim$omim_id[which(omim$Gene_name == genename())])
    if (length(a) > 1){
      a<-a[1]
    }
    paste0("https://www.omim.org/entry/", a)
  })

  annotationFile<-reactive({
    suppressMessages(AnnotationDbi::select(Homo.sapiens, keys=genename(), columns=c("SYMBOL","GO","TERM"), keytype="SYMBOL"))
  })

  goAnnotName<-reactive({
    if (!is.na(annotationFile()$GO)){
      paste0("https://www.ebi.ac.uk/QuickGO/term/", annotationFile()$GO)
    }
    else {
      paste("There is no GO annotation information")
    }
  })

  goAnnotLink<-reactive({
    paste("<a href=",goAnnotName(), "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", annotationFile()$TERM)
  })

  goAnnotAll<-reactive({
    if (!is.na(annotationFile()$GO)) {
      paste(goAnnotLink(), collapse = " | ")
    }
    else {
      paste("There is no GO annotation information")
    }
  })

  OMIMName<-reactive({
    a<-as.character(omim$omim_id[which(omim$Gene_name == genename())])
    mim<-lst[[a]]$mim
    paste0("https://www.omim.org/entry/", mim)
  })

  OMIMLink<-reactive({
    a<-as.character(omim$omim_id[which(omim$Gene_name == genename())])
    phe<-lst[[a]]$phe
    paste("<a href=",OMIMName(), "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", phe)
  })

  OMIMAll<-reactive({
    if (length(as.character(omim$omim_id[which(omim$Gene_name == genename())])) != 0){
      paste(OMIMLink(), collapse = " | ")
    }
    else {
      paste("No disease information")
    }
  })

  inputscore<-reactive({

    if (length(final_score_table$Weighted_total_scores[which(final_score_table$Gene_name == genename())]) == 0){
      paste("There is no score for",genename(), "gene.")
    }
    else {
      as.numeric(format(round(final_score_table$Weighted_total_scores[which(final_score_table$Gene_name == genename())], 3), nsmall = 3))
    }
  })

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

  output$textgeneid<-renderText({

    paste(
      "<table style=\"font-size:17px\">", "<col style=\"width: 20%;\"/>", "<col style=\"width: 80%;\"/>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Gene name:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", genename(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Gene description:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", genedescription(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "NCBI gene ID:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "</b>", "<a href=", paste0("https://www.ncbi.nlm.nih.gov/gene/",geneid()),"target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", geneid(),  "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Ensembl ID:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "</b>", "<a href=", paste0("https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",geneensembl()),"target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", geneensembl(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Synonyms:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", genesynonyms(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "GO terms:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", goAnnotAll(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "OMIM:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", omim_link(),"target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", omim_link(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "OMIM Disease:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", OMIMAll(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "CilioGenics score:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", inputscore(), "</td>", "</tr>",

      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "<b>", "Probability of being ciliary gene:", "</td>", "<td style=\"padding:0 0 10px 15px;\">", inputscorestate(), "</td>", "</tr>",

      "<table>")
  })

  output$bargeneinfo<-renderUI({
    req(input$geneName)
    tagList(
      genebar("pb", inputseq(), "Overall percentile", 1165, 1930),
      genebar("pb1", inputseq1(), "Protein interaction", 2194, 8438),
      genebar("pb2", inputseq2(), "Genetic interaction", 285, 317),
      genebar("pb3", inputseq3(), "Single cell", 1868, 2923),
      genebar("pb4", inputseq4(), "Cluster", 4845, 7759),
      genebar("pb5", inputseq5(), "Motif", 702, 1878),
      genebar("pb6", inputseq6(), "Publication", 7302, 11690),
      genebar("pb7", inputseq7(), "Protein Atlas", 651, 1016)
    )
  })
  
  polartable<-reactive({
    data.frame(cat = colnames(df_n1),
               data = t(df_n1[which(final_score_table$Gene_name == genename()),]))
  })
  
  hchart1<-reactive({
    highchart() %>% 
      hc_chart(polar = TRUE) %>% 
      hc_title(text = "Scores in Categories") %>% 
      hc_xAxis(categories = polartable()$cat,
               #tickmarkPlacement = "on",
               lineWidth = 0) %>% 
      hc_yAxis(#gridLineInterpolation = "polygon",
        lineWidth = 0,
        min = 0,
        max = 1) %>%
      hc_series(
        list(
          name = "Score",
          data = as.numeric(polartable()$data),
          pointPlacement = "on",
          colorByPoint = TRUE,
          type = "column",
          colors = ifelse(polartable()$data >= 0.75, "#27AE60", ifelse(polartable()$data <= 0.5, "#d35400","#3498DB"))
        )
      )
  })
  
  output$polarscores<-renderHighchart({
    hchart1()
  })

  ### Protein interaction page ----
  networkdata<-reactive({

    a<-biogrid %>% filter(toupper(Gene_name_A) == toupper(genename()))
    b<-intact %>% filter(toupper(Gene_name_A) == toupper(genename()))
    c<-wbP %>% filter(toupper(Gene_name_A) == toupper(genename()))
    zz<-data.frame(rbind(a, b, c))
    colnames(zz)<-c("Interactor A", "Interactor B", "Source")

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

  output$networkplot<-renderSimpleNetwork({
    simpleNetwork(networkdata(), height = "200px", width = "200px", zoom = TRUE,
                  opacity = 3, fontSize = 12)
  })

  output$pro_int<-renderReactable({
    reactable(networkdata(), resizable = TRUE, filterable = TRUE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
  })

  output$protable<-renderUI({
    box(
      width = 12,
      title = paste("Genes interacting with ", genename(), "gene"),
      solidHeader = TRUE,
      status = "success",
      withSpinner(reactableOutput("pro_int"), type = 8, color = "#10c891")
    )
  })

  output$pro_box1 <- renderUI({
    box(
      title = paste0("Protein interaction network for ", genename(), " gene"),
      width = 12,
      solidHeader = TRUE,
      status = "success",
      if (length(networkdata()[[1]]) == 0){
        textOutput("textForPro")
      }
      else {
        withSpinner(simpleNetworkOutput("networkplot"), type = 8, color = "#10c891")
      }
    )
  })

  output$textForPro<-renderText({
    paste("There is no protein interaction for", genename(), "gene")
  })

  ### Publication page ----
  pubdata<-reactive({
    a<-data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))]))
    b<-merge(a, publ, by = "Publication")
    b$Link <- paste0("<a href='",b$Link,"' target='_blank'>",b$Link,"</a>")
    b
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

  output$puberror<-renderText({
    paste("There is no publication data for", genename(), "gene")
  })

  output$pubui<-renderUI({
    if (length(unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))])) > 0){
      reactableOutput("pubtable")
    }
    else {
      h4(textOutput("puberror"))
    }
  })

  output$pubheatmap<-renderPlot({
    req(genename())
    Heatmap(as.matrix(rbind(pub_mat[pub_mat$Gene_name == genename(),-1], pub_mat3[,-1])),
            rect_gp = gpar(col = "white", lwd = 2), row_split = c("", rep(" ", 11)), row_gap = unit(5, "mm"),
            cluster_row_slices = FALSE, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun,
            height = unit(8, "cm"), show_heatmap_legend = FALSE, column_names_side = "top", row_names_side = "left",
            heatmap_height = unit(1.5, "npc"))
  })

  ### Phylogeny page ----
  inputcluster<-reactive({
    a<-as.matrix(nscores2[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]),2:73])
    rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))])]
    if (length(a[,1]) > 500){
      a<-a[1:500,]
    }
    a
  })

  inputclusterGene<-reactive({
    a<-as.matrix(nscores2[which(nscores2$Gene_name == toupper(genename())),2:73])
    a<-rbind(a,a)
    rownames(a)<-c(nscores2$Gene_name[which(nscores2$Gene_name == toupper(genename()))],"")
    a
  })

  inputclusternamenumber<-reactive({
    nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]
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

  selected3 <- reactive(getReactableState("hclustertable", "selected"))

  genenumbercluster<-reactive({
    inputclustertable()[[1]][selected3()]
  })

  inputclustergname<-reactive({
    input$hmapradio
  })

  reactiveHeatmap1<-reactive({
    #isolate({selected3()})
    #req(genename())
    #inputclustergname()
    #isolate(inputcluster())
    #isolate(inputclusterGene())
    mmap<-main_heatmap(inputcluster(), layout = list(paper_bgcolor='transparent'),
                       tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
      #add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 9))%>%
      add_col_labels(size = 0.46, font = list(family = c("open_sansregular"), size = 12), textangle=90,
                     tickvals = c(1:length(colnames(inputcluster()))))%>%
      add_col_annotation(annotation=anot, side="top", size = 0.1) %>%
      modify_layout(list(margin = list(l = 80)))

    mmap2<-main_heatmap(inputclusterGene(), layout = list(paper_bgcolor='transparent'),
                        tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
      add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 12))%>%
      add_col_labels(size = 1, font = list(family = c("open_sansregular"), size = 12), textangle=90,
                     tickvals = c(1:length(colnames(inputcluster()))))%>%
      add_col_annotation(annotation=anot, side="top", size = 0.1)

    if(input$hmapradio == "clusterradbut"){
      mmap
    }
    else {
      mmap2
    }
  })

  output$heatmapcluster<-renderIheatmap({
    reactiveHeatmap1()
  })
  
  output$clusterui<-renderUI({
    div(
      style = "position: relative",
      column(
        id = "colcolcol",
        width = 10,
        align = "center",
        offset = 1,
        box(
          width = 12,
          solidHeader = TRUE,
          status = "success",
          title = paste("Cluster", inputclusternamenumber()),
          height = "750px",
          div(
            id = "button1",
            style = "position: absolute; left: 2em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "hmapradio",
                label = "Genes to display:",
                choiceNames = c("Cluster", genename()),
                choiceValues = c("clusterradbut", "generadbut"),
                direction = "vertical",
                selected = "clusterradbut"
              ),
              size = "xm",
              icon = icon("gear", class = "opt"),
              up = TRUE,
              inputId = "ddownid"
            )
          ),
          div(
            id = "button2",
            style = "position: absolute; left: 8em;bottom: 0.5em;",
            dropdown(
              downloadButton(outputId = "hmap1", label = "Download Plot"),
              size = "xm",
              icon = icon("download", class = "opt"),
              up = TRUE
            )
          ),
          withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "630px"), type = 8, color = "#10c891"),
          br(),br(),br()
        )
      )
    )
  })

  output$hclustertable<-renderReactable({
    reactable(inputclustertable(), resizable = TRUE, filterable = TRUE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
              highlight = TRUE,
              columns = list(
                Score = colDef(name = "Score",
                               format = colFormat(digits = 3),
                               align = "left",
                               header = with_tooltip2("Score", "Single cell scores")),
                Gene_name = colDef(name = "Gene name",
                                   header = with_tooltip2("Gene name", "HGNC gene name")),
                `Gold standard` = colDef(header = with_tooltip2("Gold standard", "Is Gold standard gene?")),
                CilioGenics = colDef(header = with_tooltip2("CilioGenics", "Is the gene in the ciliary gene list of CilioGenics?"))
              ),
              rowStyle = list(cursor = "pointer"),
              selection = "single",
              onClick = "select")
  })

  output$clustertableui<-renderUI({
    column(
      width = 10,
      offset = 1,
      align = "center",
      box(
        title = paste("Genes in cluster", inputclusternamenumber()),
        solidHeader = TRUE,
        status = "success",
        width = 12,
        withSpinner(reactableOutput("hclustertable"), type = 8, color = "#10c891")
      )
    )
  })

  reactiveDownload1<-reactive({
    if (inputclustergname() == "clusterradbut"){
      filename = paste0("cluster_", inputclusternamenumber(), "_heatmap.png")
    }
    else {filename = paste0(genename(), "_heatmap.png")}
    filename
  })

  output$hmap1<-downloadHandler(
    filename = function() {
      reactiveDownload1()
    },
    content = function(file) {
      withProgress(
        message = "Downloading heatmap",
        value = 0,
        {
          shiny::incProgress(2/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          save_iheatmap(reactiveHeatmap1(), file, vwidth=1200,vheight=700)
          shiny::incProgress(3/10)
        }
      )
    },
    contentType = "image/png"
  )

  ### Single cell page ----
  
  source.list2<-reactive({
    sc.paper.list$data[sc.paper.list$paper == input$scsource2]
  })
  
  output$scumapgeneral2<-renderPlot({
    req(input$scsource2)
    DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE)
  })
  
  output$scmapgene<-renderPlotly({
    plot<-FeaturePlot(object = eval(parse(text = source.list2())), features = genename())
    HoverLocator(plot = plot, information = FetchData(eval(parse(text = source.list2())), vars = c("ident","nFeature_RNA","nCount_RNA")))
  })
  
  output$vlngene<-renderPlot({
    VlnPlot(eval(parse(text = source.list2())), features = genename())
  })
  
  output$scInputUI2<-renderUI({
    fluidRow(
      div(
        id = "scselectUI2",
        column(
          width = 6,
          align = "center",
          offset = 3,
          h3("Select a single cell RNA-seq data to visualize cell groups and gene expressions"),
          br(),
          pickerInput(
            inputId = "scsource2",
            label = "Select source of scRNA-seq data",
            choices = list(
              "Carraro et al(2021) - Lung",
              "Reyfman et al(2018) - Lung"
            ),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          )
        )
      )
    )
  })
  
  output$scUI2<-renderUI({
    if (is.null(input$scsource2)){
      
    }
    else {
      div(
        id = "scUIdiv2",
        fluidRow(
          box(
            width = 12,
            column(
              width = 6,
              plotOutput("scumapgeneral2",height = "600px") %>% withSpinner(type = 8),
              #bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
            ),
            column(
              width = 6,
              plotlyOutput("scmapgene", height = "600px") %>% withSpinner(type = 8),
              #bsTooltip("scgeneinput", "Select a gene to display its expression across cells", placement = "top")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            column(
              width = 12,
              plotOutput("vlngene") %>% withSpinner(type = 8)
            )
          )
        )
        # fluidRow(
        #   column(
        #     width = 8,
        #     align = "center",
        #     offset = 2,
        #     br(),br(),
        #     h4("Select a cluster to explore differentially expressed genes"),
        #     box(
        #       width = 12,
        #       uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
        #       reactableOutput("scmaptable"),
        #       bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
        #     )
        #   )
        # )
      )
    }
  })
  
  # r_scmainheatmap<-reactive({
  #   a<-as.matrix(celegans_sc[which(celegans_sc$tree == celegans_sc$tree[which(toupper(celegans_sc$Human_gene_name) == toupper(genename()))]),2:28])
  #   rownames(a)<-celegans_sc$Human_gene_name[which(celegans_sc$tree == celegans_sc$tree[which(toupper(celegans_sc$Human_gene_name) == toupper(genename()))])]
  #   a
  # })
  # 
  # r_scgeneheatmap<-reactive({
  #   a<-as.matrix(celegans_sc[which(celegans_sc$Human_gene_name == toupper(genename())),2:28])
  #   a<-rbind(a,a)
  #   rownames(a)<-c(celegans_sc$Human_gene_name[which(celegans_sc$Human_gene_name == toupper(genename()))],"")
  #   a
  # })
  # 
  # r_sctwoheatmap<-reactive({
  #   #req(input$geneName)
  #   #r_scgeneradio()
  #   #hmap <- isolate(r_scmainheatmap())
  #   #hmap2 <- isolate(r_scgeneheatmap())
  #   if(toupper(genename()) %in% toupper(celegans_sc$Human_gene_name)){
  #     mmap<-main_heatmap(r_scmainheatmap(), layout = list(paper_bgcolor='transparent'),
  #                        tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #       #add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 9))%>%
  #       add_col_labels(size = 0.46, font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                      tickvals = c(1:length(colnames(r_scmainheatmap()))))%>%
  #       add_col_annotation(annotation = anot_sc, colors = "Paired", side="top", size = 0.1) %>%
  #       modify_layout(list(margin = list(l = 80)))
  # 
  #     mmap2<-main_heatmap(r_scgeneheatmap(), layout = list(paper_bgcolor='transparent'),
  #                         tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #       add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 12))%>%
  #       add_col_labels(size = 1, font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                      tickvals = c(1:length(colnames(r_scmainheatmap()))))%>%
  #       add_col_annotation(annotation=anot_sc, colors = "Paired", side="top", size = 0.1)
  #   }
  # 
  #   if(input$scclusterradio == "clusterradbut1"){
  #     mmap
  #   }
  #   else {
  #     mmap2
  #   }
  # })
  # 
  # r_scclusternumber<-reactive({
  #   celegans_sc$tree[which(toupper(celegans_sc$Human_gene_name) == toupper(genename()))]
  # })
  # 
  # r_scgeneradio<-reactive({
  #   input$scclusterradio
  # })
  # 
  # observeEvent(input$scclusterradio, {
  #   session$sendCustomMessage("close_drop1", "")
  # })
  # 
  # observeEvent(input$scdownloadbttn, {
  #   session$sendCustomMessage("close_drop1", "")
  # })
  # 
  # observeEvent(input$scclusterheatmap, {
  #   session$sendCustomMessage("close_drop2", "")
  # })
  # 
  # output$scheatmapcluster<-renderIheatmap({
  #   #input$scclusterPage
  #   r_sctwoheatmap()
  # })
  # 
  # output$scclusterui<-renderUI({
  #   div(
  #     style = "position: relative",
  #     column(
  #       align = "center",
  #       id = "scclusterheatmap",
  #       width = 10,
  #       offset = 1,
  #       box(
  #         width = 12,
  #         solidHeader = TRUE,
  #         status = "success",
  #         title = paste("Cluster", r_scclusternumber()),
  #         height = "750px",
  #         div(
  #           style = "position: absolute; left: 2em; bottom: 0.5em;",
  #           dropdown(
  #             radioGroupButtons(
  #               inputId = "scclusterradio",
  #               label = "Genes to display:",
  #               choiceNames = c("Cluster", genename()),
  #               choiceValues = c("clusterradbut1", "generadbut1"),
  #               direction = "vertical",
  #               selected = "clusterradbut1"
  #             ),
  #             size = "xm",
  #             icon = icon("gear", class = "opt"),
  #             up = TRUE,
  #             inputId = "ddownid1"
  #           )
  #         ),
  #         div(
  #           id = "scdownloadbttnid",
  #           style = "position: absolute; left: 8em;bottom: 0.5em;",
  #           #dropdown(
  #             downloadButton(outputId = "scdownloadbttn", label = "Download Plot")
  #           #   size = "xm",
  #           #   icon = icon("download", class = "opt"),
  #           #   up = TRUE
  #           # )
  #         ),
  #         #tableOutput("xxx"),
  #         withSpinner(iheatmaprOutput("scheatmapcluster", width = "100%", height = "630px"), type = 8, color = "#10c891"),
  #         br(),br(),br()
  #       )
  #     )
  #   )
  # })
  # 
  # reactiveDownload2<-reactive({
  #   if (r_scgeneradio() == "clusterradbut1"){
  #     filename = paste0("cluster_", r_scclusternumber(), "_heatmap.png")
  #   }
  #   else {filename = paste0(genename(), "_heatmap.png")}
  #   filename
  # })
  # 
  # output$scdownloadbttn<-downloadHandler(
  #   filename = function() {
  #     reactiveDownload2()
  #   },
  #   content = function(file) {
  #     withProgress(
  #       message = "Downloading heatmap",
  #       value = 0,
  #       {
  #         shiny::incProgress(2/10)
  #         Sys.sleep(1)
  #         shiny::incProgress(5/10)
  #         save_iheatmap(r_sctwoheatmap(), file, vwidth=1200,vheight=700)
  #         shiny::incProgress(3/10)
  #       }
  #     )
  #   },
  #   contentType = "image/png"
  # )
  # 
  # # Table
  # r_scclustertable<-reactive({
  # 
  #   df<-data.frame(celegans_sc[which(celegans_sc$tree == celegans_sc$tree[which(toupper(celegans_sc$Human_gene_name) == toupper(genename()))]),29], stringsAsFactors = FALSE)
  #   colnames(df)<-"Gene_name"
  #   df$Score<-final_score_table$Weighted_total_scores[match(df[[1]], final_score_table$Gene_name)]
  #   df$`Gold standard` <- "NO"
  #   df$`Gold standard`[which(df[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
  #   df$CilioGenics <- "NO"
  #   df$CilioGenics[which(df[[1]] %in% ciliogenics[[1]])]<-"YES"
  #   df
  # })
  # 
  # 
  # output$scclustertable<-renderReactable({
  #   reactable(r_scclustertable(), resizable = TRUE, filterable = TRUE,
  #             searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
  #             highlight = TRUE,
  #             columns = list(
  #               Score = colDef(name = "Score",
  #                              format = colFormat(digits = 3),
  #                              align = "left",
  #                              header = with_tooltip2("Score", "Single cell scores")),
  #               Gene_name = colDef(name = "Gene name",
  #                                  header = with_tooltip2("Gene name", "HGNC gene name")),
  #               `Gold standard` = colDef(header = with_tooltip2("Gold standard", "Is Gold standard gene?")),
  #               CilioGenics = colDef(header = with_tooltip2("CilioGenics", "Is the gene in the ciliary gene list of CilioGenics?"))
  #             ),
  #             rowStyle = list(cursor = "pointer"),
  #             selection = "single",
  #             onClick = "select")
  # })
  # 
  # output$scclustertableui<-renderUI({
  #   column(
  #     width = 10,
  #     offset = 1,
  #     align = "center",
  #     box(
  #       title = paste("Genes in cluster", r_scclusternumber()),
  #       solidHeader = TRUE,
  #       status = "success",
  #       width = 12,
  #       withSpinner(reactableOutput("scclustertable"), type = 8, color = "#10c891")
  #     )
  #   )
  # })

  # Explore gene tab ----
  
  output$toggle<-renderUI({
    if (!input$isMobile){
      div(
        id = "toggleui",
        column(
          width = 1,
          style = "background-color: #00bcd4; border-radius: 15px 0 0 15px;",
          actionButton("toggleSidebar1", icon("th"), style = "padding-top: 20px; padding-bottom: 12px;")
        ),
        column(
          width = 11,
          align = "center",
          style = "background-color: #00bcd4; border-radius: 0 15px 15px 0; padding-top: 60px; padding-bottom: 12px;"#6f7dc8
        )
      )
    }
    else {}
  })
  
  ### General score ----
  genenumber<-reactive({
    final_score_table$Gene_name[selected()]
  })

  genenumber2<-reactive({
    final_seq_table$Gene_name[selected2()]
  })

  selected <- reactive(getReactableState("generaltable2", "selected"))
  selected2 <- reactive(getReactableState("generaltable3", "selected"))

  output$generaltable2<-renderReactable({

    reactable(final_score_table, resizable = TRUE, filterable = TRUE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
              columns =list(
                Gene_name = colDef(header = with_tooltip("Gene name",
                                                         "HGNC name of the genes"),
                                   format = colFormat(digits = 3)),
                Protein_interaction_score = colDef(header = with_tooltip("Protein interactions",
                                                                         "Protein interaction scores"),
                                                   format = colFormat(digits = 3)),
                Genetic_interaction_score = colDef(header = with_tooltip("Genetic interactions",
                                                                         "Genetic interaction scores"),
                                                   format = colFormat(digits = 3)),
                Single_cell_score = colDef(header = with_tooltip("Single cell",
                                                                 "Single cell scores. Based on expression in ciliated cells"),
                                           format = colFormat(digits = 3)),
                Cluster_score = colDef(header = with_tooltip("Phylogenetic analysis",
                                                             "Phylogenetic analysis scores. Based on presence in ciliary organisms"),
                                       format = colFormat(digits = 3)),
                Motif_score = colDef(header = with_tooltip("Motifs",
                                                           "Motif scores. Based on the presence of ciliary motifs on the gene (Refer to this)."),
                                     format = colFormat(digits = 3)),
                Publication_score = colDef(header = with_tooltip("Publications",
                                                                 "Publication scores. Based on the publications that the gene is reported."),
                                           format = colFormat(digits = 3)),
                Protein_atlas_score = colDef(header = with_tooltip("Protein atlas",
                                                                   "Protein atlas scores. Based on the presence of ciliary features in the gene reported in Protein Atlas"),
                                             format = colFormat(digits = 3)),
                Total_score = colDef(header = with_tooltip("Total raw scores",
                                                           "Total unscaled raw scores"),
                                     format = colFormat(digits = 3)),
                Norm_total_score = colDef(header = with_tooltip("Normalized score",
                                                                "Total normalized scores"),
                                          format = colFormat(digits = 3)),
                Weighted_total_scores = colDef(header = with_tooltip("Weighted score",
                                                                     "Total weighted scores. Weights are based on the success rate of finding ciliary genes."),
                                               format = colFormat(digits = 3)),
                Seq = colDef(header = with_tooltip("Order",
                                                   "Order of the genes based on weighted scores"))
              ),
              rowStyle = list(cursor = "pointer"),
              selection = "single",
              onClick = "select"
    )
  })
  
  # observeEvent(selected(),{
  #   updateTabItems(inputId = "tabs", selected = "hometab")
  # })

  ### Phylogeny section ----
  inputclusternumber<-reactive({
        a<-as.matrix(nscores2[which(nscores2$cluster_number == input$clusternumber),2:73])
        rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == input$clusternumber)]
        a
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

  selected4 <- reactive(getReactableState("hclusternumbertable", "selected"))

  genenumbergeneralcluster<-reactive({
    inputclusternumbertable()[[1]][selected4()]
  })

  heatmapclusternumberR<-reactive({
    main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'),
                 tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
      add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90,
                     tickvals = c(1:length(colnames(inputclusternumber()))))%>%
      add_col_annotation(annotation=anot, side="top", size = 0.1) %>%
      modify_layout(list(margin = list(l = 80)))
  })

  output$heatmapclusternumber<-renderIheatmap({
    heatmapclusternumberR()
  })
  
  reactiveDownload3<-reactive({
    filename = paste0("cluster_", input$clusternumber, "_heatmap.png")
  })
  
  output$hmap3<-downloadHandler(
    filename = function() {
      reactiveDownload3()
    },
    content = function(file) {
      # showModal(modalDialog("Downloading heatmap", footer=NULL))
      # on.exit(removeModal())
      # save_iheatmap(heatmapclusternumberR(), file, vwidth=1200,vheight=600)
      shiny::withProgress(
        message = "Downloading heatmap",
        value = 0,
        {
          shiny::incProgress(2/10)
          Sys.sleep(1)
          shiny::incProgress(5/10)
          save_iheatmap(heatmapclusternumberR(), file, vwidth=1200,vheight=700)
          shiny::incProgress(3/10)
        }
      )
    },
    contentType = "image/png"
  )

  ### Single cell ----
  # r_scclusternumber2<-reactive({
  #   a<-as.matrix(celegans_sc[which(celegans_sc$tree == input$clusternumber2),2:28])
  #   rownames(a)<-celegans_sc$Human_gene_name[which(celegans_sc$tree == input$clusternumber2)]
  #   a
  # })
  # 
  # r_scgenenumber<-reactive({
  #   b<-celegans_sc[which(celegans_sc$tree == input$clusternumber2),]
  #   a<-as.matrix(b[which(b$Human_gene_name == toupper(input$clusternumber3)),2:28])
  #   a<-rbind(a,a)
  #   rownames(a)<-c(b$Human_gene_name[which(b$Human_gene_name == toupper(input$clusternumber3))],"")
  #   a
  # })
  # 
  # r_scclusternumbertable<-reactive({
  #   a<-data.frame(celegans_sc[which(celegans_sc$tree == input$clusternumber2),29])
  #   colnames(a)<-"Gene_name"
  #   a$Score<-final_score_table$Weighted_total_scores[match(a[[1]], final_score_table$Gene_name)]
  #   a$`Gold standard` <- "NO"
  #   a$`Gold standard`[which(a[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
  #   a$CilioGenics <- "NO"
  #   a$CilioGenics[which(a[[1]] %in% ciliogenics[[1]])]<-"YES"
  #   a
  # })

  # output$schclusternumbertable<-renderReactable({
  #   reactable(r_scclusternumbertable(), resizable = TRUE, filterable = TRUE,
  #             columns = list(Score = colDef(format = colFormat(digits = 3)),
  #                            Gene_name = colDef(name = "Gene name")),
  #             defaultPageSize = 10, showPageSizeOptions = TRUE,
  #             highlight = TRUE)
  # })

  # scheatmapclusternumberR<-reactive({
  #   main_heatmap(r_scclusternumber2(), layout = list(paper_bgcolor='transparent'),
  #                tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #     #add_row_labels(size = 0.03,font = list(family = c("open_sansregular"), size = 7))%>%
  #     add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                    tickvals = c(1:length(colnames(r_scclusternumber2()))))%>%
  #     add_col_annotation(annotation=anot_sc, side="top", size = 0.1) %>%
  #     modify_layout(list(margin = list(l = 80)))
  # })

  # scheatmapgenenumberR<-reactive({
  #   main_heatmap(r_scgenenumber(), layout = list(paper_bgcolor='transparent'),
  #                tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #     #add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 12))%>%
  #     add_col_labels(size = 1, font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                    tickvals = c(1:length(colnames(r_scgenenumber()))))%>%
  #     add_col_annotation(annotation=anot_sc, side="top", size = 0.1) %>%
  #     modify_layout(list(margin = list(l = 80)))
  # })

  # output$scheatmapclusternumber<-renderIheatmap({
  #   if (input$clusternumber3 == "All"){
  #     scheatmapclusternumberR()
  #   }
  #   else if(input$clusternumber3 == ""){
  #     scheatmapclusternumberR()
  #   }
  #   else {
  #     scheatmapgenenumberR()
  #   }
  # })
  
  
  # genelistforpicker<-reactive({
  #   a<-c(celegans_sc$Human_gene_name[which(celegans_sc$tree == input$clusternumber2)])
  #   a<-sort(a)
  #   a
  # })

  # output$pickeroutput<-renderUI({
  #   pickerInput(
  #     inputId = "clusternumber3",
  #     label = "Select a gene to explore",
  #     choices = list(
  #       "Gene name" = c("All", genelistforpicker())
  #     ),
  #     selected = "All",
  #     options=pickerOptions(liveSearch=T)
  #   )
  # })
  
  #### Single cell maps ----
  
  # UI #
  output$scInputUI<-renderUI({
    fluidRow(
      div(
        id = "scselectUI",
        column(
          width = 6,
          align = "center",
          offset = 3,
          h3("Select a single cell RNA-seq data to visualize cell groups and gene expressions"),
          br(),
          pickerInput(
            inputId = "scsource",
            label = "Select source of scRNA-seq data",
            choices = list(
              "Carraro et al(2021) - Lung",
              "Reyfman et al(2018) - Lung"
            ),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          )
        )
      )
    )
  })
  
  output$scUI<-renderUI({
    if (is.null(input$scsource)){
      
    }
    else {
      div(
        id = "scUIdiv",
        # fluidRow(
        #   column(
        #     width = 6,
        #     offset = 6,
        #     uiOutput("scgeneinput"),
        #     uiOutput("scgenebutton")
        #   )
        # ),
        fluidRow(
          box(
            width = 12,
            column(
              width = 6,
              plotOutput("scumapgeneral",height = "600px") %>% withSpinner(type = 8),
              bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
            ),
            column(
              width = 6,
              fluidRow(
                column(
                  width = 6,
                  uiOutput("scgeneinput")
                ),
                column(
                  width = 3,
                  uiOutput("scgenebutton")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  plotOutput("dotgene") %>% withSpinner(type = 8)
                )
              )
            )
            # column(
            #   width = 6,
            #   plotOutput("heatmapgene") %>% withSpinner(type = 8),
            # )
          )
        ),
        fluidRow(
          column(
            width = 8,
            align = "center",
            offset = 2,
            br(),br(),
            h4("Select a cluster to explore differentially expressed genes"),
            box(
              width = 12,
              uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
              reactableOutput("scmaptable"),
              bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
            )
          )
        )
      )
    }
  })
  
  # output$scUI2<-renderUI({
  #   fluidRow(
  #     column(
  #       width = 9,
  #       uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
  #       reactableOutput("scmaptable"),
  #       bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
  #     )
  #   )
  # })
  
  # observeEvent(input$scsource,{
  #   if (is.null(input$scsource)){
  #     hide("scUIdiv")
  #   }
  #   else {
  #     show("scUIdiv")
  #   }
  # })
  
  
  # Server #
  
  source.list<-reactive({
    sc.paper.list$data[sc.paper.list$paper == input$scsource]
  })
  
  output$scumapgeneral<-renderPlot({
    req(input$scsource)
    DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE)
  })
  
  # output$scgeneinput<-renderUI({
  #   req(input$scsource)
  #   pickerInput(
  #     inputId = "scgene",
  #     label = "Select a gene",
  #     choices = rownames(eval(parse(text = source.list()))),
  #     selected = NULL,
  #     multiple = TRUE,
  #     options = pickerOptions(maxOptions = 1,
  #                             liveSearch = TRUE)
  #   )
  # })
  
  output$scgeneinput<-renderUI({
    req(input$scsource)
    pickerInput(
      inputId = "scgene",
      label = "Select multiple genes",
      choices = rownames(eval(parse(text = source.list()))),
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(liveSearch = TRUE)
    )
  })
  
  output$scgenebutton<-renderUI({
    req(input$scsource)
    actionButton("scbttn","Draw",icon = "thumbs-up")
  })
  
  # output$scgeneinput<-renderUI({
  #   req(input$scsource)
  #   if (input$scsource == "Carraro et al(2021) - Lung"){
  #     autocomplete_input(
  #       id = "scgene",
  #       label = "Select a gene",
  #       options = rownames(lung),
  #       max_options = 50
  #     )
  #   }
  # })
  
  # observeEvent(input$scsource, {
  #   update_autocomplete_input(
  #     session,
  #     "scgene",
  #     options = rownames(lung)
  #   )
  # })
  
  #updateSelectizeInput(session, "scgene", choices = rownames(lung), server = TRUE)
  
  # output$scmapgene<-renderPlot({
  #   req(input$scgene)
  #   plot<-FeaturePlot(object = eval(parse(text = source.list())), features = input$scgene)
  #   HoverLocator(plot = plot, information = FetchData(reyfmans.reduced, vars = c("ident","nFeature_RNA","nCount_RNA")))
  # })
  
  output$dotgene<-renderPlot({
    req(input$scsource)
    req(input$scgene)
    req(input$scbttn)
    input$scbttn
    DotPlot(eval(parse(text = source.list())), features = input$scgene)
  })
  
  output$heatmapgene<-renderPlot({
    req(input$scgene)
    #req(input$scbttn)
    input$scbttn
    DoHeatmap(subset(eval(parse(text = source.list())), downsample = 100), features = input$scgene, size = 3)
  })
  
  output$sccelltypeinput<-renderUI({
    req(input$scsource)
    pickerInput(
      inputId = "sccelltypes",
      label = "Select cluster to get gene list",
      choices = c(levels(eval(parse(text = source.list())))),
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(maxOptions = 1),
      width = "40%"
    )
    
  })
  
  diff_list<-reactive({
    a<-levels(eval(parse(text = source.list())))
    i<-which(a == input$sccelltypes)
    if (source.list() == "lung"){
      lung_markers[[i]]
    }
    else if (source.list() == "reyfman"){
      reyfman_markers[reyfman_markers$cluster == input$sccelltypes,]
    }
  })
  
  output$sometext<-renderText({
    
    a<-levels(eval(parse(text = source.list())))
    i<-which(a == input$sccelltypes)
    paste("i is:", i)
  })
  
  output$scmaptable<-renderReactable({
    req(input$sccelltypes)
    reactable(
      diff_list(), resizable = TRUE, filterable = TRUE,
      searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
      highlight = TRUE
    )
  })
  
  # output$scmaptable<-renderDT({
  #   #req(input$sccelltypes)
  #   as.data.table(diff_list())
  # })
  
  # tippy
  
  output$tippy1<-renderUI({
    column(
      width = 1,
      br(),br(),
      tippy(
        bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
        "<span style='font-size:15px;'>Select a source to visualize the cells!<span>",
        placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
      )
    )
  })
  output$tippy2<-renderUI({
    req(input$scsource)
    column(
      width = 1,
      br(),br(),
      tippy(
        bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
        "<span style='font-size:15px;'>Select a gene to display its expression across cells.<span>",
        placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
      )
    )
  })
  output$tippy3<-renderUI({
    req(input$scsource)
    column(
      width = 1,
      br(),br(),
      tippy(
        bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
        "<span style='font-size:15px;'>Select a group to list genes differentially expressed in that group.<span>",
        placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
      )
    )
  })


  ### Publications ----
  pubheatmapx<-reactive({
    req(input$pubgene)
    main_heatmap(as.matrix(pubgenelist_mat()), layout = list(paper_bgcolor='transparent'),
                 tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
      add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90)%>%
      modify_layout(list(margin = list(b = 200)))

  })

  output$pubgeneralheatmap<-renderIheatmap({
    pubheatmapx()
  })

  output$selectgene<-renderText("Please select a gene to visualize associated publications!")

  output$pubgeneralheatmapUi<-renderUI({
    req(input$pubgene)
    if(input$pubgene == ""){
      textOutput("selectgene")
    }
    else {
      withSpinner(iheatmaprOutput("pubgeneralheatmap"), type = 8)
    }
  })

  pubgenelist_mat<-reactive({
    if(input$pubgene != "All"){
      pub_genes[pub_genes$Gene_name == input$pubgene,-1]
    }
  })

  output$pubpickeroutput<-renderUI({
    fluidRow(
      id = "pubpicker",
      column(
        width = 3,
        pickerInput(
          inputId = "pubgene",
          label = "Select a gene",
          choices = list(
            "Gene name" = c("", pub_genes$Gene_name)
          ),
          selected = "",
          options=pickerOptions(liveSearch=T)
        )
      ),
      column(
        width = 1,
        br(),
        tippy(bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "big"),
              "<span style='font-size:15px;'>Please select a gene to visualize associated publications!<span>",
              placement = "right", animation = "scale", arrow = TRUE, theme = "blue")
      )
    )
  })
  
  output$pubchart<-renderHighchart({
    hchart(xc3, type = "line", hcaes(x = Publication, y = `Number of genes`, group = type)) %>%
      hc_tooltip(shared = TRUE)
  })
  
  gene_pub_list<-reactive({
    xc4[xc4$Publication == input$pubpub,c(1,5,6)]
  })
  
  output$pubselecttable<-renderReactable({
    reactable(gene_pub_list(), resizable = TRUE, filterable = TRUE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
              highlight = TRUE
    )
  })
  
  
  
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$toggleSidebar1, {
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  })
  
  observeEvent(input$toggleSidebar2, {
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  })
  
  # Others ----

  waiter_hide()
}
