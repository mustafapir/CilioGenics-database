
library(shiny)

source("functions.R")

theme <- reactableTheme(
  style = list(color = "#fff", background = "#282a36"),
  cellStyle = list(borderColor = "rgba(255, 255, 255, 0.15)"),
  headerStyle = list(borderColor = "rgba(255, 255, 255, 0.15)"),
  paginationStyle = list(borderColor = "rgba(255, 255, 255, 0.15)"),
  rowHighlightStyle = list(background = "rgba(255, 255, 255, 0.04)"),
  pageButtonHoverStyle = list(background = "rgba(255, 255, 255, 0.08)"),
  pageButtonActiveStyle = list(background = "rgba(255, 255, 255, 0.1)")
)

# Server ----
server <- function(input, output, session){
  
  # get cookies
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
  
  # Auto navigating
  values <- reactiveValues(
    
    autoNavigating = 0,
    pageinput = 0,
    searchString = ""
    
  )
  
  observeEvent(session$clientData$url_search, {
    # if there is a history in the URL, restore the state
    if (nchar(session$clientData$url_search) > 1) {
      # when the app starts, the input$navbar gets triggered, but we don't
      # want to trigger the navigation function because the user didn't actively
      # navigate anywhere
      values$autoNavigating <- values$autoNavigating
      
      restore(session$clientData$url_search)
    }
  })
  
  restore <- function(qs) {
    data <- parseQueryString(qs)
    
    if (!is.null(data[['page']])) {
      
      values$autoNavigating <- values$autoNavigating
      
      if (data[['page']] == "Home"){
        hide("nvbr")
        hide("inpt")
      } else {
        updateSearchInput(session, "geneName", value = data[['query']], trigger = TRUE)
        updateNavbarPage(session, inputId = "nvbr", selected = data[['page']])
        show("inpt")
      }
    }
  }
  
  observeEvent(input$nvbr, {
    if (values$autoNavigating > 0) {
      values$autoNavigating <- values$autoNavigating - 1
      return()
    }
    
    if (input$geneName != "") {
      shinyjs::js$updateHistory(page = input$nvbr, query = input$geneName)
    }
    else {
      shinyjs::js$updateHistory(page = input$nvbr)
    }
    
  })
  
  observeEvent(input$navigatedTo, {
    restore(input$navigatedTo)
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
  
  observeEvent(input$helpbutton, {
    
    if (input$nvbr == "Home"){
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
    }
    else if(input$nvbr == "General info"){
      guide2$init()$start()
    }
    else if(input$nvbr == "Interactions"){
      guide3$init()$start()
    }
    else if(input$nvbr == "Phylogenetic analysis"){
      guide4$init()$start()
    }
    else if(input$nvbr == "Single cell"){
      guide5$init()$start()
    }
    else if(input$nvbr == "Publications"){
      guide6$init()$start()
    }
    else if(input$nvbr == "Motifs"){
      guide7$init()$start()
    }
    
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
    if (toupper(trimmedGname()) %in% homsap$ensembl_gene_id){
      gname<-unique(homsap$Gene_name[which(homsap$ensembl_gene_id %in% toupper(trimmedGname()))])
    }
    else if (trimmedGname() %in% homsap$entrez_id){
      gname<-unique(homsap$Gene_name[which(homsap$entrez_id %in% trimmedGname())])
    }
    else {
      if (!(toupper(trimmedGname()) %in% toupper(homsap$Gene_name)) &&
          (toupper(trimmedGname()) %in% toupper(homsap$`Alias symbol`)) &&
          length(unique(homsap$Gene_name[toupper(homsap$`Alias symbol`) %in% toupper(trimmedGname())])) == 1) {
        gname<-unique(homsap$Gene_name[which(toupper(homsap$`Alias symbol`) %in% toupper(trimmedGname()))])
      }
      else if (toupper(trimmedGname()) %in% toupper(homsap$Gene_name)){
        gname<-unique(homsap$Gene_name[which(toupper(homsap$Gene_name) %in% toupper(trimmedGname()))])
      }
      else if (trimmedGname() %in% orthology$Gene1Symbol){
        gname<-unique(orthology$Gene2Symbol[which(orthology$Gene1Symbol %in% trimmedGname())])
      }
      else if (trimmedGname() %in% orthology$Gene1ID){
        gname<-unique(orthology$Gene2Symbol[which(orthology$Gene1ID %in% trimmedGname())])
      }
      else {gname<-trimmedGname()}
    }
    gname
  })
  
  # genename<-reactive({
  #   input$geneName
  # })
  
  # List of genes having same synonym gene names
  # geneoption<-reactive({
  #   c(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname()))])
  # })
  
  # Show only landing page
  observeEvent("", {
    
    hide("nvbr")
    hide("inpt")
    values$pageinput<-0
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
    else if (length(genename()) > 1){
      showModal(
        modalDialog(
          h3("Multiple Results"),
          radioGroupButtons(
            "generadio",
            h4("It appears there are multiple genes corresponding to the input. Please select one: "),
            #geneoption(),
            genename(),
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
      show("nvbr")
      show("inpt")
      if (values$pageinput == 0){
        updateNavbarPage(inputId = "nvbr", selected = "General info")
      }
      values$pageinput<-values$pageinput + 1
      values$searchString <- input$geneName
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
        mm<-customnumber()-1
        customnumber(mm)
      }
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
  
  
  observeEvent(input$geneName2_search, {
    if (input$geneName2 == ""){
      
    }
    else if (length(genename()) > 1){
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
    updateTabItems(session, "nvbr", selected = "General info")
    show("nvbr")
    show("inpt")
  })
  
  observeEvent(input$ARL13B, {
    session$sendCustomMessage("geneName", "ARL13B")
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })
  
  observeEvent(input$IFT74, {
    session$sendCustomMessage("geneName", "ENSG00000096872")
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })
  
  observeEvent(input$BBS5, {
    session$sendCustomMessage("geneName", "129880")
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
  })
  
  observeEvent(input$`che-3`, {
    session$sendCustomMessage("geneName", "che-3")
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")
    
  })
  
  observeEvent(input$birc5b, {
    session$sendCustomMessage("geneName", "birc5b")
    delay(100,
          showModal(
            modalDialog(
              h3("Multiple Results"),
              radioGroupButtons(
                "generadio",
                h4("It appears there are multiple genes corresponding to the input. Please select one: "),
                #geneoption(),
                genename(),
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
    )
  })
  
  observeEvent(input$FBgn0052751, {
    session$sendCustomMessage("geneName", "FBgn0052751")
    delay(100,
          showModal(
            modalDialog(
              h3("Multiple Results"),
              radioGroupButtons(
                "generadio",
                h4("It appears there are multiple genes corresponding to the input. Please select one: "),
                genename(),
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
    )
  })
  
  
  observeEvent(input$nvbr, {
    if (input$nvbr == "Home"){
      values$pageinput<-0
      hide("nvbr")
      hide("inpt")
      click("geneName2_reset")
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
        show("nvbr")
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = TRUE)
  
  genenumber2<-reactive({
    final_score_table$Gene_name[selected2()]
  })
  
  observeEvent(genenumber2(), {
    if (length(genenumber2() != "") != 0){
      if (genenumber2() != ""){
        session$sendCustomMessage("geneName", genenumber2())
        updateNavbarPage(inputId = "nvbr", selected = "General info")
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
  
  # Mobile specific design for a better readability
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
  
  geneinfo_server("ginfo", genename)
  
  geneinfo_server("ginfo1", genename)
  
  geneinfo_server("ginfo2", genename)
  
  geneinfo_server("ginfo3", genename)
  
  geneinfo_server("ginfo4", genename)
  
  
  #### Gene info ---------------------------------------------
  geneid<-reactive({
    unique(homsap$entrez_id[which(homsap$Gene_name == genename())])
  })
  
  genedescription<-reactive({
    unique(homsap$`Approved name`[which(homsap$Gene_name == genename())])
  })
  
  genesynonyms<-reactive({
    a<-unique(homsap$`Alias symbol`[which(homsap$Gene_name == genename())])
    if (length(a) > 1){
      a<-paste0(a, collapse = "|\n")
    }
    a
  })
  
  geneensembl<-reactive({
    a<-unique(homsap$ensembl_gene_id[which(homsap$Gene_name == genename())])
    if (length(a) > 1){
      a<-paste0(a, collapse = "|\n")
    }
    a
  })
  
  omim_link<-reactive({
    a<-unique(as.character(homsap$omim_id[which(homsap$Gene_name == genename())]))
    if (length(a) > 1){
      a<-a[1]
    }
    if (length(a) == 0){
      paste("There is no OMIM entry for this gene")
    }
    else {
      paste0("https://www.omim.org/entry/", a)
    }
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
    a<-unique(as.character(homsap$omim_id[which(homsap$Gene_name == genename())]))
    mim<-lst[[a]]$mim
    paste0("https://www.omim.org/entry/", mim)
  })
  
  OMIMLink<-reactive({
    a<-unique(as.character(homsap$omim_id[which(homsap$Gene_name == genename())]))
    phe<-lst[[a]]$phe
    paste("<a href=",OMIMName(), "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", phe)
  })
  
  OMIMAll<-reactive({
    if (length(as.character(homsap$omim_id[which(homsap$Gene_name == genename())])) != 0){
      paste(OMIMLink(), collapse = " | ")
    }
    else {
      paste("No disease information")
    }
  })
  
  inputscore<-reactive({
    if (length(final_score_table$Mean_score[which(final_score_table$Gene_name == genename())]) == 0){
      paste("There is no score for",genename(), "gene.")
    }
    else {
      as.numeric(format(round(final_score_table$Mean_score[which(final_score_table$Gene_name == genename())], 3), nsmall = 3))
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
  
  
  output$textgeneid<-renderText({
    
    paste(
      "<table id='genetable' style=\"font-size:17px\">", "<col style=\"width: 20%;\"/>", "<col style=\"width: 80%;\"/>",
      
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
  
  
  #### General page charts----
  
  polartable<-reactive({
    db<-load_data_mysql()
    scores<-tbl(db, "new_scores_all") %>%
      select(Gene_name, normalized_sc_score, normalized_protein_score, normalized_genetic_score,
             normalized_motif_score, normalized_phylogenetic_scores, normalized_pub_score, normalized_pa_score) %>%
      filter(Gene_name == local(genename())) %>%
      collect()
    dbDisconnect(db)
    colnames(scores)<-colnames(final_score_table)[1:8]
    scores<-round(scores[,2:8], digits = 3)
    scores<-scores[,c(2,1,4,3,5:7)]
    
    data.frame(cat = colnames(scores),
               data = t(scores))
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
          colors = ifelse(polartable()$data >= 0.4, "#27AE60", ifelse(polartable()$data <= 0.2, "#d35400","#3498DB"))
        )
      )
  })
  
  output$polarscores<-renderHighchart({
    hchart1()
  })
  
  observe({
    cat(input$GetScreenWidth)
  })
  
  output$polarUI<-renderUI({
    if(as.numeric(input$GetScreenWidth) > 1600){
      highchartOutput("polarscores", height = "450px")
    }
    else {
      div(
        br(),
        highchartOutput("polarscores", height = "300px"),
        br(),br()
      )
    }
  })
  
  
  ## Protein interaction page ----
  
  networkdata<-reactive({
    
    db<-load_data_mysql()
    prot.int<-tbl(db, "protein_interactions") %>%
      filter(`Interactor A` == local(genename()) & Source %in% local(input$proradio)) %>%
      select(`Interactor A`,`Interactor B`,`Interaction detection method(s)`,Author,`Publication Source`,Source,
             `Original name A`, `Original name B`) %>%
      collect()
    dbDisconnect(db)
    prot.int
  })
  
  
  networkplotdata<-reactive({
    
    x<-networkdata()
    
    x$is.ciliary<-"Unknown"
    x$is.ciliary[which(x[[1]] %in% ciliaryGenes1$Gene.Name)]<-"Ciliary"
    x$is.ciliary2<-"Unknown"
    x$is.ciliary2[which(x[[2]] %in% ciliaryGenes1$Gene.Name)]<-"Ciliary"
    x<-x[!is.na(x$`Interactor B`),]
    
    glist<-data.frame(Gene_name = unique(c(x$`Interactor A`, x$`Interactor B`)),
                      Gene_number = c(1:length(unique(c(x$`Interactor A`, x$`Interactor B`))))-1)
    
    x$number_A<-glist$Gene_number[match(x$`Interactor A`, glist$Gene_name)]
    x$number_B<-glist$Gene_number[match(x$`Interactor B`, glist$Gene_name)]
    
    p.links.d3<-data.frame(from = x$number_A, to = x$number_B)
    
    x1<-x%>%select(`Interactor A`, is.ciliary)
    x2<-x%>%select(`Interactor B`, is.ciliary2)
    colnames(x1)<-c("Gene_name","is.ciliary")
    colnames(x2)<-c("Gene_name","is.ciliary")
    x3<-unique(rbind(x1,x2))
    
    p.nodes.d3<-cbind(idn=factor(x3$Gene_name, levels=x3$Gene_name), x3)
    
    forceNetwork(Links = p.links.d3, Nodes = p.nodes.d3, Source="from", Target="to",
                 NodeID = "Gene_name", Group = "is.ciliary",
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), linkColour = "#afafaf",
                 fontSize=15, zoom=F, legend=T, opacity = 0.6, charge=-30)
    
  })
  
  output$networkplot<-renderSimpleNetwork({
    networkplotdata()
  })
  
  output$pro_int<-renderReactable({
    reactable(networkdata(), height = 500, resizable = TRUE, filterable = FALSE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
  })
  
  
  output$pro_box1 <- renderUI({
    if (length(networkdata()[[1]]) == 0){
      textOutput("textForPro")
    }
    else {
      simpleNetworkOutput("networkplot")
    }
  })
  
  reactiveDownload4<-reactive({
    filename = paste0(genename(), "_protein_interactions.csv")
  })
  
  output$prot.table<-downloadHandler(
    filename = function() {
      reactiveDownload4()
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading table",
        value = 0,
        {
          shiny::incProgress(7/10)
          Sys.sleep(1)
          shiny::incProgress(3/10)
          write.csv(networkdata(), file = file)
          shiny::incProgress(2/10)
        }
      )
    },
    contentType = "csv"
  )
  
  
  # Genetic interactions
  genenetworkdata<-reactive({
    
    db<-load_data_mysql()
    gene.int<-tbl(db, "genetic_interactions") %>%
      filter(`Interactor A` == local(genename()) & Source %in% local(input$proradio)) %>%
      select(`Interactor A`,`Interactor B`,`Interaction detection method(s)`,Author,`Publication Source`,Source,
             `Original name A`,`Original name B`) %>%
      collect()
    dbDisconnect(db)
    gene.int
  })
  
  genenetworkplotdata<-reactive({
    
    x<-genenetworkdata()
    
    x$is.ciliary<-"Unknown"
    x$is.ciliary[which(x[[1]] %in% ciliaryGenes1$Gene.Name)]<-"Ciliary"
    x$is.ciliary2<-"Unknown"
    x$is.ciliary2[which(x[[2]] %in% ciliaryGenes1$Gene.Name)]<-"Ciliary"
    x<-x[!is.na(x$`Interactor B`),]
    
    glist<-data.frame(Gene_name = unique(c(x$`Interactor A`, x$`Interactor B`)),
                      Gene_number = c(1:length(unique(c(x$`Interactor A`, x$`Interactor B`))))-1)
    
    x$number_A<-glist$Gene_number[match(x$`Interactor A`, glist$Gene_name)]
    x$number_B<-glist$Gene_number[match(x$`Interactor B`, glist$Gene_name)]
    
    p.links.d3<-data.frame(from = x$number_A, to = x$number_B)
    
    x1<-x%>%select(`Interactor A`, is.ciliary)
    x2<-x%>%select(`Interactor B`, is.ciliary2)
    colnames(x1)<-c("Gene_name","is.ciliary")
    colnames(x2)<-c("Gene_name","is.ciliary")
    x3<-unique(rbind(x1,x2))
    
    p.nodes.d3<-cbind(idn=factor(x3$Gene_name, levels=x3$Gene_name), x3)
    
    forceNetwork(Links = p.links.d3, Nodes = p.nodes.d3, Source="from", Target="to",
                 NodeID = "Gene_name", Group = "is.ciliary",linkWidth = 1,
                 colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"), linkColour = "#afafaf",
                 fontSize=12, zoom=F, legend=T, opacity = 0.6, charge=-30)
  })
  
  output$genenetworkplot<-renderSimpleNetwork({
    genenetworkplotdata()
  })
  
  output$gene_int<-renderReactable({
    reactable(genenetworkdata(), height = 500, resizable = TRUE, filterable = FALSE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE)
  })
  
  output$gene_box <- renderUI({
    
    if (length(genenetworkdata()[[1]]) == 0){
      textOutput("textForGene")
    }
    else {
      simpleNetworkOutput("genenetworkplot")
    }
  })
  
  output$textForPro<-renderText({
    paste("There is no protein interaction for", genename(), "gene")
  })
  
  output$textForGene<-renderText({
    paste("There is no genetic interaction for", genename(), "gene")
  })
  
  reactiveDownload5<-reactive({
    filename = paste0(genename(), "_genetic_interactions.csv")
  })
  
  output$gene.table<-downloadHandler(
    filename = function() {
      reactiveDownload5()
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading table",
        value = 0,
        {
          shiny::incProgress(7/10)
          Sys.sleep(1)
          shiny::incProgress(3/10)
          write.csv(genenetworkdata(), file = file)
          shiny::incProgress(2/10)
        }
      )
    },
    contentType = "csv"
  )
  
  
  ### Publication page ----
  pubdata<-reactive({
    db<-load_data_mysql()
    pub<-tbl(db, "publications") %>%
      filter(Gene_name %in% local(genename())) %>%
      select(Publication) %>%
      collect()
    publ<-tbl(db, "publication_list") %>%
      collect()
    dbDisconnect(db)
    b<-merge(pub, publ, by = "Publication")
    if (length(b[[1]]) > 0){
      b$Link <- paste0("<a href='",b$Link,"' target='_blank'>",b$Paper,"</a>")
    }
    b
  })
  
  
  output$pubtable<-renderReactable({
    if(input$darkmode == TRUE){
      reactable(
        pubdata()[,c(-2,-7)], resizable = TRUE, filterable = TRUE,
        searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
        highlight = TRUE,
        columns = list(
          Link = colDef(html = TRUE)
        ),
        theme = theme
      )
    } else {
      reactable(
        pubdata()[,c(-2,-7)], resizable = TRUE, filterable = TRUE,
        searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
        highlight = TRUE,
        columns = list(
          Link = colDef(html = TRUE)
        )
      )
    }
  })
  
  
  
  output$puberror<-renderText({
    paste("There is no publication data for", genename(), "gene")
  })
  
  output$pubui<-renderUI({
    if (length(pubdata()[[1]]) > 0){
      reactableOutput("pubtable")
    }
    else {
      h4(textOutput("puberror"))
    }
  })
  
  pub_mat<-reactive({
    db<-load_data_mysql()
    pub<-tbl(db, "pub_mat", encoding = "UTF-8") %>%
      filter(Gene_name %in% local(genename())) %>%
      collect()
    pub2<-tbl(db, "pub_mat", encoding = "UTF-8") %>%
      filter(Gene_name %in% pubgenelist) %>%
      collect()
    pub<-rbind(pub, pub2)
    dbDisconnect(db)
    pub<-data.frame(pub, check.names = FALSE)
    rownames(pub)<-pub[[1]]
    pub<-pub[,2:ncol(pub)]
  })
  
  
  output$pubheatmap<-renderPlot({
    req(genename())
    Heatmap(as.matrix(pub_mat()),
            rect_gp = gpar(col = "white", lwd = 2), row_split = c("", rep(" ", 11)), row_gap = unit(5, "mm"),
            cluster_row_slices = FALSE, cluster_rows = FALSE, cluster_columns = FALSE, col = col_fun,
            height = unit(8, "cm"), show_heatmap_legend = FALSE, column_names_side = "top", row_names_side = "left",
            heatmap_height = unit(1.5, "npc"))
  })
  
  
  output$pubexpui<-renderUI({
    h4("This page shows publications among 52 cilia related papers in which ", genename(), " gene is published.
                   In the heatmap, 11 other genes which are published by the highest amount of publication are also shown.")
  })
  
  
  ### Motif page ----
  motiftable<-reactive({
    unique(motifs[motifs$`Gene name` == genename(),6:9])
  })
  
  output$motiftbl<-renderReactable({
    reactable(motiftable(), columns = list(
      `Motif ID` = colDef(name = "Consensus sequence",
                          cell = function(value) {
                            image <- img(src = sprintf("motifs/%s.jpg", value), height = "100px", width = "220px", alt = value)
                            tagList(
                              div(style = list(display = "inline-block", width = "220px"), image)
                            )
                          },
                          header = with_tooltip3("Consensus sequence",
                                                 "Consensus sequence taken from MotifMap")
      ),
      
      Motif = colDef(name = "Motif",
                     style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                     header = with_tooltip3("Motif", "Name of the motif")
      ),
      
      `Motif ID` = colDef(name = "Motif ID"),
      
      BBLS = colDef(name = "BBLS",
                    style = function(value) {
                      bar_style(width = value / max(motifs$BBLS), fill = "#2c5e77", color = "#fff")
                    },
                    align = "left",
                    format = colFormat(digits = 3),
                    header = with_tooltip3("BBLS",
                                           "Bayesian Branch Length Score (BBLS) is assessment of the degree of evolutionary conservation")),
      `Distance(bp)` = colDef(name = "Distance(bp)",
                              style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                              header = with_tooltip3("Distance (bp)",
                                                     "Motif's distance from the gene (upstream)"))
    ),
    bordered = TRUE,
    sortable = FALSE)
  })
  
  ### Phylogeny page ----
  inputcluster<-reactive({
    validate(
      need(nscores2$Gene_name %in% genename(), sprintf("No data for %s gene", genename()))
    )
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
    validate(
      need(nscores2$Gene_name %in% genename(), sprintf("No data for %s gene", genename()))
    )
    
    df<-data.frame('Gene name' = nscores2[which(nscores2$cluster_number == nscores2$cluster_number[which(toupper(nscores2$Gene_name) == toupper(genename()))]),1], stringsAsFactors = FALSE)
    df$Score<-final_score_table$Mean_score[match(df[[1]], final_score_table$Gene_name)]
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
    
    mmap3<-main_heatmap(inputcluster(), layout = list(paper_bgcolor='transparent'),
                        tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
      #add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 9))%>%
      add_col_labels(size = 0.46, font = list(family = c("open_sansregular"), size = 11), textangle=90,
                     tickvals = c(1:length(colnames(inputcluster()))))%>%
      add_col_annotation(annotation=anot, side="top", size = 0.1) %>%
      modify_layout(list(margin = list(l = 80)))
    
    
    if(input$hmapradio == "clusterradbut" & as.numeric(input$GetScreenWidth) >= 1600){
      mmap
    }
    else if (input$hmapradio == "clusterradbut" & as.numeric(input$GetScreenWidth) < 1600){
      mmap3
    }
    else {
      mmap2
    }
  })
  
  
  output$heatmapcluster<-renderIheatmap({
    reactiveHeatmap1()
  })
  
  
  output$clusterui<-renderUI({
    if (!(genename() %in% nscores2$Gene_name)){
      h3(sprintf("No data for %s gene", genename()))
    }
    else {
      div(
        style = "position: relative",
        column(
          id = "colcolcol",
          width = 12,
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            title = paste("Cluster", inputclusternamenumber()),
            height = "750px",
            # h4("This page shows the results of comparative genomics analysis which includes genome of
            #          72 different species and 60 clusters grouping genes that are conserved in the same species."),
            
            h3(tags$b("Interactive Heatmap of Comparative Genomics between ciliated and non-ciliated cells"),
               style = "margin-left:50px;"),
            br(),
            h5(paste0("* The heatmap shows the cluster in which human ", genename(), " gene belongs to.
                      Y axis represents genes while X axis shows organisms. Genes having ortholog in an organism shows
                      dark color. The list of all human genes in each cluster can be explored in the bottom page."),
               style = "margin-left:50px; line-height:1.5;"),
            h5("* You can also plot heatmap for only gene of interest by selecting 'Toggle display'",
               style = "margin-left:50px;"),
            # h5("Explore the comparative genomics of a wide range of ciliated and non-ciliated organisms.
            #    There are 60 distinct clusters depending on distributions of genes across the species.
            #    Please select the cluster and view the interactive heatmap.
            #    The list of all human genes in each cluster can be explored in the bottom page.",
            #    style = "line-height: 1.5;"),
            div(
              id = "button1",
              style = "position: absolute; left: 2em; bottom: 0.5em;",
              dropdown(
                #inputId = "drpbttn",
                radioGroupButtons(
                  inputId = "hmapradio",
                  label = "Genes to display:",
                  choiceNames = c("Whole cluster", paste0("Only ",genename())),
                  choiceValues = c("clusterradbut", "generadbut"),
                  direction = "vertical",
                  selected = "clusterradbut"
                ),
                size = "xm",
                icon = icon("gear", class = "opt"),
                up = TRUE,
                inputId = "ddownid",
                label = "Toggle display"
              )
            ),
            div(
              id = "button2",
              style = "position: absolute; left: 15em;bottom: 0.5em;",
              downloadButton(outputId = "hmap1", label = "Download Plot")
            ),
            withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "630px"), type = 8),
            br(),br(),br(),
            h5()
          )
        )
      )
    }
  })
  
  output$hclustertable<-renderReactable({
    reactable(inputclustertable()[,-4], resizable = TRUE, filterable = TRUE,
              searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
              highlight = TRUE,
              columns = list(
                Score = colDef(name = "CilioGenics score",
                               format = colFormat(digits = 3),
                               align = "left",
                               header = with_tooltip2("Score", "Single cell scores")),
                Gene_name = colDef(name = "Gene name",
                                   header = with_tooltip2("Gene name", "HGNC gene name")),
                `Gold standard` = colDef(header = with_tooltip2("Gold standard", "Is Gold standard gene?"))
              ))
  })
  
  output$clustertableui<-renderUI({
    if (!(genename() %in% nscores2$Gene_name)){
      
    }
    else {
      column(
        width = 12,
        #offset = 1,
        #align = "center",
        box(
          id = "cluster_box",
          title = paste("Genes in cluster", inputclusternamenumber()),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          h4("Gene list which appears in the selected cluster. List can be filtered
                           (i.e. for only gold standard genes, type 'Yes' in gold standard column."),
          withSpinner(reactableOutput("hclustertable"), type = 8)
        )
      )
    }
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
  
  feature.names<-reactive({
    input$scsource2
    if(input$scsource2 == "Cao et al(2017) - C. elegans"){
      x<-isolate(orthology$Gene1Symbol[orthology$Gene2Symbol == genename() & orthology$Gene1SpeciesName == "Caenorhabditis elegans"])
    } else if (input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      x<-isolate(unique(mouse_orthology2$Gene_name[which(mouse_orthology2$`Approved symbol` == genename())]))
    }
    else {x<-isolate(genename())}
    x
  })
  
  source.list2<-reactive({
    if (input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      req(input$pandorasource2)
      x<-sc.paper.list$data[sc.paper.list$paper == input$scsource2]
      y<-eval(parse(text = x))
      out<-y$data[y$paper == input$pandorasource2]
    } else {
      out<-sc.paper.list$data[sc.paper.list$paper == input$scsource2]
    }
    out
  })
  
  output$scumapgeneral2_binding<-renderPlot({
    req(input$scsource2)
    #input$geneName
    if(input$scsource2 == "Cao et al(2017) - C. elegans"){
      if(length(feature.names())>1){
        req(input$celeinput)
        ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
          geom_point(size = 0.05) +
          #guides(colour = guide_legend(override.aes = list(size=2))) +
          ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
          theme_classic() +
          theme(legend.position = "none")
      }
      else{
        ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
          geom_point(size = 0.05) +
          #guides(colour = guide_legend(override.aes = list(size=2))) +
          ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
          theme_classic() +
          theme(legend.position = "none")
      }
    }
    else if(input$scsource2 == "Habermann et al(2020) - Lung (human)"){
      DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE, repel = TRUE, raster = FALSE) +
        NoLegend()
    }
    else if(input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      req(input$pandorasource2)
      DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE)
    }
    else{
      DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE)
    }
  })
  
  celeinput1<-reactive({
    input$celeinput
  })
  
  scmapgeneplot<-reactive({
    req(input$scsource2)
    input$scsource2
    if(input$scsource2 == "Cao et al(2017) - C. elegans"){
      if(length(feature.names())>1){
        req(input$celeinput)
        p<-isolate(plot.expr(cds, celeinput1(), cell_size = 0.05) + theme(legend.position = "right"))
      }
      else{
        p<-isolate(plot.expr(cds, feature.names(), cell_size = 0.05) + theme(legend.position = "right"))
      }
    }
    else if (input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      req(input$pandorasource2)
      input$pandorasource2
      p<-isolate(FeaturePlot(object = eval(parse(text = source.list2())), features = feature.names(), pt.size = 0.05))
    }
    else {
      p<-isolate(FeaturePlot(object = eval(parse(text = source.list2())), features = feature.names(), pt.size = 0.05))
    }
    p
  })
  
  
  output$scmapgene<-renderPlot({
    input$scsource2
    input$pandorasource2
    p<-isolate(scmapgeneplot())
    p
  })
  
  
  output$vlngene<-renderPlot({
    req(input$scsource2)
    input$scsource2
    if(input$scsource2 == "Cao et al(2017) - C. elegans" & !is.null(input$celeinput)){
      req(input$celeinput)
      p<-isolate(VlnPlot(eval(parse(text = source.list2())), features = input$celeinput))
    }
    else if(input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)" & 
            !is.null(input$pandorasource2)){
      req(input$pandorasource2)
      input$pandorasource2
      p<-isolate(VlnPlot(eval(parse(text = source.list2())), features = feature.names()))
    }
    else{
      p<-isolate(VlnPlot(eval(parse(text = source.list2())), features = feature.names()))
    }
    p
  })
  
  
  dfforvln<-reactive({
    if(length(feature.names())>1){
      row.names(subset(cele_genes, symbol == input$celeinput))
    }
    else{
      row.names(subset(cele_genes, symbol == feature.names()))
    }
  })
  
  
  output$vlngene2<-renderPlot({
    plot_genes_jitter(cds[dfforvln(),], grouping = "cell.type", color_by = "cell.type", nrow= 1,
                      ncol = NULL, plot_trend = TRUE) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), legend.position = "none")
  })
  
  
  output$scInputUI2<-renderUI({
    input$geneName
    fluidRow(
      div(
        id = "scselectUI2",
        column(
          width = 6,
          align = "center",
          offset = 3,
          h3(tags$b(paste0("Select a single cell RNA-seq data to visualize cell groups and expression profile of ",
                           genename(),
                           " gene"))),
          br(),
          pickerInput(
            inputId = "scsource2",
            label = "Select source of scRNA-seq data",
            choices = list(
              "Carraro et al(2021) - Lung (human)",
              "Reyfman et al(2018) - Lung (human)",
              "Habermann et al(2020) - Lung (human)",
              "Chen et al(2021) - Lung(mammals, reptiles, birds)",
              "Cao et al(2017) - C. elegans"
            ),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          ),
          uiOutput("pandora_options"),
          uiOutput("cele_options")
        )
      )
    )
  }) %>% bindCache(input$geneName)
  
  observeEvent(input$nvbr, {
    if(input$nvbr == "Home"){
      hide("scUIdiv2")
      reset("scsource2")
      reset("scmapgene")
      reset("singlegeneexp")
      reset("vlngene")
      updatePickerInput(session = session, "scsource2", selected = NULL)
      click("geneName_reset")
    }
  })
  
  output$pandora_options<-renderUI({
    req(input$scsource2)
    if(input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      div(
        h5("Select an organism"),
        pickerInput(
          inputId = "pandorasource2",
          label = "Select the organism",
          choices = list(
            "Tiger Lung","Pangolin Lung","Deer Lung","Goat Lung","Rabbit Lung","Cat Lung",
            "Dog Lung","Hamster Lung","Lizard Lung","Duck Lung","Pigeon Lung","Bat Lung"
          ),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        )
      )
    }
    else {}
  })
  
  output$cele_options<-renderUI({
    req(input$scsource2)
    if(input$scsource2 == "Cao et al(2017) - C. elegans" & length(feature.names())>1){
      div(
        h5(sprintf("There are multiple orthologs for %s gene in C. elegans. Select one:", genename())),
        pickerInput(
          inputId = "celeinput",
          label = "Select a C. elegans gene",
          choices = feature.names(),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        )
      )
    }
    else {}
  })
  
  output$singlegeneexp<-renderUI({
    input$scsource2
    if(input$scsource2 == "Cao et al(2017) - C. elegans"){
      if(length(feature.names()) > 1){
        req(input$celeinput)
      }
      else if(length(feature.names()) == 0){
        h4(paste0("There is no ortholog of ", genename(), " gene in C. elegans"))
      }
      p<-isolate(plotOutput("scmapgene", height = "600px") %>% withSpinner(type = 8))
    }
    else {
      p<-isolate(plotOutput("scmapgene", height = "600px") %>% withSpinner(type = 8))
    }
  })
  
  
  output$scUI2<-renderUI({
    req(input$scsource2)
    if (is.null(input$scsource2)){
      
    }
    else if(input$scsource2 == "Chen et al(2021) - Lung(mammals, reptiles, birds)" &
            is.null(input$pandorasource2)){
      
    }
    else if(input$scsource2 == "Cao et al(2017) - C. elegans"){
      if(length(feature.names())>1 & is.null(input$celeinput)){
        
      }
      else if (length(feature.names())==0){
        h4(paste0("There is no ortholog of ", genename(), " gene in C. elegans"))
      }
      else {
        div(
          id = "scUIdiv2",
          fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              status = "success",
              h4(paste0("The plot on the left is a t-SNE plot showing all cells for the selected single cell RNA-seq data.
                 The plot on the right is a t-SNE plot showing only cells expressing ", feature.names(), " gene. The plot
                         at the bottom of the page shows expression levels of gene of interest in different cell types"),
                 style = "margin-left: 50px;"),
              br(),
              column(
                width = 6,
                plotOutput("scumapgeneral2_binding", height = "600px") %>% withSpinner(type = 8)
                #bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
              ),
              column(
                width = 6,
                uiOutput("singlegeneexp")
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              solidHeader = TRUE,
              status = "success",
              column(
                width = 12,
                plotOutput("vlngene2", height = "500px") %>% withSpinner(type = 8)
              )
            )
          )
        )
      }
    }
    
    else{
      div(
        id = "scUIdiv2",
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            h4(paste0("The plot on the left is an UMAP plot showing all cells for the selected single cell RNA-seq data.
                 The plot on the right is an UMAP plot showing only cells expressing ", genename()," gene. The violin plot
                         at the bottom of the page shows expression levels of gene of interest in different cell types"),
               style = "margin-left: 50px;"),
            h5("* Clusters having a number as cluster name represent unidentified clusters.",
               style = "margin-left: 50px;"),
            column(
              width = 6,
              plotOutput("scumapgeneral2_binding", height = "600px") %>% withSpinner(type = 8)
              #bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
            ),
            column(
              width = 6,
              uiOutput("singlegeneexp")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            column(
              width = 12,
              plotOutput("vlngene") %>% withSpinner(type = 8)
            )
          )
        )
      )
    }
  }) 
  
  
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
    final_score_table$Gene_name[selected2()]
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
                Phylogenetic_tree_score = colDef(header = with_tooltip("Phylogenetic analysis",
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
                Mean_score = colDef(header = with_tooltip("Mean score",
                                                          "Total mean scores"),
                                    format = colFormat(digits = 3)),
                Order = colDef(header = with_tooltip("Order",
                                                     "Order of the genes based on weighted scores"))
              ),
              rowStyle = list(cursor = "pointer"),
              selection = "single",
              onClick = "select"
    )
  })
  
  
  ### Phylogeny section ----
  inputclusternumber<-reactive({
    req(input$clusternumber)
    a<-as.matrix(nscores2[which(nscores2$cluster_number == input$clusternumber),2:73])
    rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == input$clusternumber)]
    a
  }) %>% bindCache(input$clusternumber)
  
  inputclusternumbertable<-reactive({
    req(input$clusternumber)
    a<-data.frame('Gene name' = nscores2[which(nscores2$cluster_number == input$clusternumber),1])
    a$Score<-final_score_table$Mean_score[match(a[[1]], final_score_table$Gene_name)]
    a$`Gold standard` <- "NO"
    a$`Gold standard`[which(a[[1]] %in% ciliaryGenes1$Gene.Name)]<-"YES"
    a$CilioGenics <- "NO"
    a$CilioGenics[which(a[[1]] %in% ciliogenics[[1]])]<-"YES"
    a
  }) %>% bindCache(input$clusternumber)
  
  output$hclusternumbertable<-renderReactable({
    reactable(inputclusternumbertable()[,-4], resizable = TRUE, filterable = TRUE, searchable = TRUE,
              columns = list(Score = colDef(name = "CilioGenics score",
                                            format = colFormat(digits = 3),
                                            align = "left",
                                            header = with_tooltip2("Score", "Single cell scores")),
                             Gene_name = colDef(name = "Gene name",
                                                header = with_tooltip2("Gene name", "HGNC gene name")),
                             `Gold standard` = colDef(header = with_tooltip2("Gold standard", "Is Gold standard gene?"))),
              defaultPageSize = 10, showPageSizeOptions = TRUE,
              highlight = TRUE)
  }) %>% bindCache(input$clusternumber)
  
  selected4<-reactive(getReactableState("hclusternumbertable", "selected"))
  
  genenumbergeneralcluster<-reactive({
    inputclusternumbertable()[[1]][selected4()]
  })
  
  heatmapclusternumberR<-reactive({
    if (as.numeric(input$GetScreenWidth) >= 1600){
      main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'),
                   tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
        add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90,
                       tickvals = c(1:length(colnames(inputclusternumber()))))%>%
        add_col_annotation(annotation=anot, side="top", size = 0.1) %>%
        modify_layout(list(margin = list(l = 80)))
    }
    else {
      main_heatmap(inputclusternumber(), layout = list(paper_bgcolor='transparent'),
                   tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Organism: "))%>%
        add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 11), textangle=90,
                       tickvals = c(1:length(colnames(inputclusternumber()))))%>%
        add_col_annotation(annotation=anot, side="top", size = 0.1) %>%
        modify_layout(list(margin = list(l = 80)))
    }
  }) %>% bindCache(inputclusternumber(), input$GetScreenWidth)
  
  output$heatmapclusternumber<-renderIheatmap({
    heatmapclusternumberR()
  }) %>% bindCache(heatmapclusternumberR())
  
  output$heatmapclusternumberUI<-renderUI({
    box(
      title = "Phylogenetic analysis",
      solidHeader = TRUE,
      status = "success",
      width = 12,
      # h4("This page shows the results of comparative genomics analysis which includes genome of
      #            72 different species and 60 clusters grouping genes that are conserved in the same species."),
      h3(tags$b("Interactive Heatmap of Comparative Genomics between ciliated and non-ciliated cells"),
         style = "margin-left: 50px;"),
      br(),
      h5("Explore the comparative genomics of a wide range of ciliated and non-ciliated organisms.
               There are 60 distinct clusters depending on distributions of genes across the species.
               Please select the cluster and view the interactive heatmap.
               The list of all human genes in each cluster can be explored in the bottom page.",
         style = "line-height: 1.5; margin-left: 50px"),
      column(
        width = 4,
        pickerInput(
          inputId = "clusternumber",
          label = "Select cluster number to explore",
          choices = list(
            "Ciliary organisms specific clusters" = c(56,58),
            "Average conservation" = c(1,4,6,9,16,30),
            "Low specificity" = c(2:3,5,7:8,10:15,17:29,31:55,57,58:60)
          )
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), type = 8),
          h5("* Y axis represents genes while X axis shows organisms. Genes having ortholog in an organism shows
                      dark color.")
        ),
        column(
          width = 12,
          align = "left",
          div(
            id = "button3",
            style = "left: 10ex;",
            downloadButton(outputId = "hmap3", label = "Download Plot")
          )
        )
      )
    )
  })
  
  output$clustergenetable<-renderUI({
    box(
      title = "Genes in cluster",
      solidHeader = TRUE,
      status = "success",
      width = 12,
      column(
        width = 12,
        
        h4("Gene list which appears in the selected cluster. List can be filtered
                           (i.e. for only gold standard genes, type 'Yes' in gold standard column.)"),
        withSpinner(reactableOutput("hclusternumbertable"), type = 8)
      ),
      column(
        width = 12,
        align = "left",
        div(
          id = "button4",
          style = "left: 10ex;",
          downloadButton(outputId = "clustergenetbl", label = "Download Table")
        )
      )
    )
  })
  
  reactiveDownload3<-reactive({
    filename = paste0("cluster_", input$clusternumber, "_heatmap.png")
  })
  
  output$hmap3<-downloadHandler(
    filename = function() {
      reactiveDownload3()
    },
    content = function(file) {
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
  
  reactiveDownload4<-reactive({
    filename = paste0("cluster_", input$clusternumber, "_genes.csv")
  })
  
  output$clustergenetbl<-downloadHandler(
    filename = function() {
      reactiveDownload4()
    },
    content = function(file) {
      shiny::withProgress(
        message = "Downloading table",
        value = 0,
        {
          shiny::incProgress(7/10)
          Sys.sleep(1)
          shiny::incProgress(2/10)
          write.csv(inputclusternumbertable()[,-4], file)
          shiny::incProgress(3/10)
        }
      )
    },
    contentType = "csv"
  )
  
  
  ### Single cell ----
  
  
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
          h3(tags$b("Select a single cell RNA-seq data to visualize cell groups and gene expressions")),
          br(),
          pickerInput(
            inputId = "scsource",
            label = "Select source of scRNA-seq data",
            choices = list(
              "Carraro et al(2021) - Lung (human)",
              "Reyfman et al(2018) - Lung (human)",
              "Habermann et al(2020) - Lung (human)",
              "Chen et al(2021) - Lung(mammals, reptiles, birds)",
              "Cao et al(2017) - C. elegans"
            ),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(maxOptions = 1)
          ),
          uiOutput("pandora_options2")
        )
      )
    )
  })
  
  output$pandora_options2<-renderUI({
    req(input$scsource)
    if(input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      div(
        h5("Select an organism"),
        pickerInput(
          inputId = "pandorasource",
          label = "Select the organism",
          choices = list(
            "Tiger Lung","Pangolin Lung","Deer Lung","Goat Lung","Rabbit Lung","Cat Lung",
            "Dog Lung","Hamster Lung","Lizard Lung","Duck Lung","Pigeon Lung","Bat Lung"
          ),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1)
        )
      )
    }
    else {}
  })
  
  output$scUI<-renderUI({
    if (is.null(input$scsource)){
      
    }
    else if(input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)" &
            is.null(input$pandorasource)){
      
    }
    else if(input$scsource == "Cao et al(2017) - C. elegans"){
      div(
        id = "scUIdiv",
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            h4("The plot on the left is t-SNE plot showing all cells for the selected single cell RNA-seq data.
                 On the right, you can type single or multiple gene names to visualize and compare their expression
               profiles on a dot plot.", style = "margin-left: 50px;"),
            style = "margin-left: 50px;",
            column(
              width = 6,
              plotOutput("scumapgeneral",height = "600px") %>% withSpinner(type = 8),
              bsTooltip("scsource", "Select a source to visualize the cells", placement = "top")
            ),
            column(
              width = 6,
              fluidRow(
                uiOutput("message"),
                column(
                  width = 6,
                  selectInput(
                    "scgene",
                    "Select multiple genes",
                    multiple = TRUE,
                    choices = NULL
                  )
                ),
                column(
                  width = 3,
                  br(),
                  uiOutput("scgenebutton")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  br(),
                  plotOutput("dotgene", height = "600px") %>% withSpinner(type = 8)
                )
              )
            )
          )
        )
      )
    }
    else {
      div(
        id = "scUIdiv",
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            status = "success",
            h4("The plot on the left is UMAP plot showing all cells for the selected single cell RNA-seq data.
                 On the right, you can type single or multiple gene names to visualize and compare their expression
               profiles on a dot plot. On the bottom of the page, differentially expressed genes and related data
               can be explored for each cluster.", style = "margin-left: 50px;"),
            h5("* Clusters having a number as cluster name represent unidentified clusters.",
               style = "margin-left: 50px;"),
            column(
              width = 6,
              plotOutput("scumapgeneral",height = "600px") %>% withSpinner(type = 8),
              bsTooltip("scsource", "Select a source to visualize the cells", placement = "top")
            ),
            column(
              width = 6,
              fluidRow(
                uiOutput("message"),
                column(
                  width = 6,
                  selectInput(
                    "scgene",
                    "Select multiple genes",
                    multiple = TRUE,
                    choices = NULL
                  )
                ),
                column(
                  width = 3,
                  br(),
                  uiOutput("scgenebutton")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  br(),
                  plotOutput("dotgene", height = "600px") %>% withSpinner(type = 8)
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 10,
            align = "center",
            br(),br(),
            box(
              width = 12,
              solidHeader = TRUE,
              status = "success",
              h4("Select a cluster to explore differentially expressed genes"),
              uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
              reactableOutput("scmaptable"),
              bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top"),
              h5("You can apply filters to any column by typing in the box below column names. Search within the table
                 is also possible.")
            )
          )
        )
      )
    }
  })  %>% bindCache(input$scsource)
  
  
  # Server #
  output$message<-renderUI({
    h4("Select a group of genes to visualize expressions in a dot plot")
  })
  
  source.list<-reactive({
    if (input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      req(input$pandorasource)
      x<-sc.paper.list$data[sc.paper.list$paper == input$scsource]
      y<-eval(parse(text = x))
      out<-y$data[y$paper == input$pandorasource]
    } else {
      out<-sc.paper.list$data[sc.paper.list$paper == input$scsource]
    }
    out
  })
  
  output$scumapgeneral<-renderPlot({
    req(input$scsource)
    if(input$scsource == "Cao et al(2017) - C. elegans"){
      ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
        geom_point(size = 0.05) +
        guides(colour = guide_legend(override.aes = list(size=2))) +
        ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
        theme_classic() +
        theme(legend.position = "none")
    }
    else if(input$scsource == "Habermann et al(2020) - Lung (human)"){
      DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE, repel = TRUE, raster = FALSE) +
        NoLegend()
    }
    else if (input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)"){
      req(input$pandorasource)
      DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE)
    }
    else{
      DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE)
    }
  })  %>% bindCache(input$scsource)
  
  # pan_choices<-reactive({
  #   if(input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)") {
  #     chs<-pandora.list$data[pandora.list$paper == input$scsource]
  #     
  #   }
  #   
  # })
  
  
  observeEvent(input$scsource, {
    if (input$scsource == "Carraro et al(2021) - Lung (human)"){
      updateSelectizeInput(session, "scgene", choices = lung_names, server = TRUE)
    }
    else if(input$scsource == "Reyfman et al(2018) - Lung (human)"){
      updateSelectizeInput(session, "scgene", choices = reyfman_names, server = TRUE)
    }
    else if(input$scsource == "Habermann et al(2020) - Lung (human)"){
      updateSelectizeInput(session, "scgene", choices = habermann_names, server = TRUE)
    }
    else if(input$scsource == "Cao et al(2017) - C. elegans"){
      updateSelectizeInput(session, "scgene", choices = cele_names, server = TRUE)
    }
    else {
      req(input$pandorasource)
      updateSelectizeInput(session, "scgene", choices = rownames(eval(parse(source.list))), server = TRUE)
    }
  })
  
  
  output$scgenebutton<-renderUI({
    req(input$scsource)
    actionButton("scbttn","Draw",icon = icon("thumbs-up"))
  })
  
  
  scgene1<-eventReactive(input$scbttn, {
    input$scbttn
    isolate(input$scgene)
  })
  
  output$dotgene<-renderPlot({
    if(input$scsource == "Cao et al(2017) - C. elegans"){
      DotPlot(eval(parse(text = source.list())), features = scgene1()) + RotatedAxis() +
        theme(axis.title.y = element_text(size=1))
    }
    else if (input$scsource == "Chen et al(2021) - Lung(mammals, reptiles, birds)") {
      req(input$pandorasource)
      DotPlot(eval(parse(text = source.list())), features = scgene1()) + RotatedAxis()
    }
    else {
      DotPlot(eval(parse(text = source.list())), features = scgene1()) + RotatedAxis()
    }
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
      width = "80%"
    )
    
  })
  
  diff_list<-reactive({
    a<-levels(eval(parse(text = source.list())))
    i<-which(a == input$sccelltypes)
    if (source.list() == "lung"){
      lung_markers[[i]]
    }
    else if (source.list() == "reyfman"){
      reyfman_markers[reyfman_markers$cluster == input$sccelltypes,-7]
    }
    else if (source.list() == "habermann"){
      habermann_markers[habermann_markers$cluster == input$sccelltypes,-7]
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
      highlight = TRUE,
      columns = list(
        gene = colDef(header = with_tooltip("Gene name", "Gene name")),
        p_val = colDef(header = with_tooltip("p_val","p_val (unadjusted)"), width = 100,
                       format = colFormat(digits = 3)),
        avg_log2FC = colDef(header = with_tooltip("avg_log2FC","log fold-chage of the average expression between the two groups.
                                          Positive values indicate that the feature is more
                                          highly expressed in the first group."),
                            format = colFormat(digits = 3)),
        pct.1 = colDef(header = with_tooltip("pct.1","The percentage of cells where the feature is detected in the first group"),
                       format = colFormat(digits = 3)),
        pct.2 = colDef(header = with_tooltip("pct.2","The percentage of cells where the feature is detected in the second group"),
                       format = colFormat(digits = 3)),
        p_val_adj = colDef(header = with_tooltip("p_val_adj","Adjusted p-value, based on bonferroni
                                     correction using all features in the dataset"),
                           format = colFormat(digits = 3))
      ),
      rownames = FALSE
    )
  })
  
  
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
  
  
  output$selectgene<-renderText("Please select a gene to visualize associated publications!")
  
  
  pub_list<-reactive({
    db<-load_data_mysql()
    df<-tbl(db, "publication_list") %>%
      #select(- Pub_number) %>%
      collect()
    df$Link <- paste0("<a href='",df$Link,"' target='_blank'>",df$Paper,"</a>")
    dbDisconnect(db)
    df
  })
  
  
  output$pubselecttable2<-renderReactable({
    reactable(pub_list()[,c(-2,-7)], resizable = FALSE, filterable = FALSE,
              searchable = TRUE, defaultPageSize = 100, showPageSizeOptions = FALSE,
              height = 500, rowStyle = list(cursor = "pointer"), selection = "single",
              onClick = "select",
              columns = list(
                Link = colDef(name = "Title",
                              html = TRUE,
                              width = 300),
                Year = colDef(width = 50)
              ))
  })
  
  numbofgene<-reactive({
    db<-load_data_mysql()
    gene_numb<-tbl(db, "pub_gene_type") %>%
      pivot_longer(cols = c(ciliary, all), names_to = "Gene type", values_to = "Number of genes") %>%
      collect()
    dbDisconnect(db)
    gene_numb
  })
  
  output$pubchart<-renderHighchart({
    hchart(numbofgene(), type = "line", hcaes(x = Publication, y = `Number of genes`, group = `Gene type`)) %>%
      hc_tooltip(shared = TRUE)
  })
  
  gene_pub_list<-reactive({
    req(selected5())
    db<-load_data_mysql()
    pubgene<-tbl(db, "publications") %>%
      filter(Pub_number == local(pubselecttable_selection())) %>%
      select(Gene_name, Gene_ID) %>%
      collect()
    pubgene<-pubgene %>%
      mutate(`Gene name` = pubgene$Gene_name ,`Original gene name` = pubgene$Gene_ID) %>%
      select(3,4)
    dbDisconnect(db)
    pubgene
  })
  
  selected5 <- reactive(getReactableState("pubselecttable2", "selected"))
  
  pubselecttable_selection<-reactive({
    pub_list()[[2]][selected5()]
  })
  
  pubselecttable_selection2<-reactive({
    pub_list()$Publication[selected5()]
  })
  
  output$pubselecttable<-renderReactable({
    #req(input$pubpub)
    reactable(gene_pub_list(), resizable = TRUE, filterable = FALSE,
              searchable = TRUE, defaultPageSize = 20, showPageSizeOptions = TRUE,
              highlight = TRUE, height = 600
    )
  })
  
  output$publicationUI1<-renderUI({
    if (is.null(selected5())){
      column(
        width = 6,
        box(
          title = sprintf("Gene list for %s", pubselecttable_selection2()),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          h4("Select a publication from the table (left) to see the published gene list")
        )
      )
    }
    else {
      column(
        width = 6,
        box(
          title = sprintf("Gene list for %s", pubselecttable_selection2()),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          reactableOutput("pubselecttable") #%>% withSpinner(type = 8)
        )
      )
    }
  })
  
  output$publicationUI2<-renderUI({
    if(as.numeric(input$GetScreenWidth) >= 1600){
      column(
        width = 12,
        box(
          title = "Number of genes",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          br(),br(),br(),br(),
          highchartOutput("pubchart") %>% withSpinner(type = 8),
          h5("* Line plot showing total reported gene number and ciliary gene number.")
        )
      )
    }
    else {
      column(
        width = 12,
        box(
          title = "Number of genes",
          solidHeader = TRUE,
          status = "success",
          width = 12,
          br(),br(),br(),br(),
          highchartOutput("pubchart") %>% withSpinner(type = 8),
          h5("* Line plot showing total reported gene number and ciliary gene number.")
        )
      )
    }
  })
  
  
  ### Motifs ----
  motiftable2<-reactive({
    unique(motifs[motifs$`Motif ID` == input$motifname, c(6,7,4,8,9)])
  })
  
  output$motifUI<-renderUI({
    pickerInput(
      inputId = "motifname",
      label = "Select motif",
      choices = list("Motifs" = motiflist),
      selected = "",
      options=pickerOptions(liveSearch=T)
    )
  })
  
  output$consUI<-renderUI({
    req(input$motifname)
    div(
      tags$img(src=sprintf("motifs/%s.jpg", input$motifname), height='150px', width='300px')
    )
  })
  
  
  output$motiftbl2<-renderReactable({
    req(input$motifname)
    reactable(motiftable2(), columns = list(
      `Motif ID` = colDef(name = "Motif id",
                          header = with_tooltip4("Motif id",
                                                 "Motif id from MotifMap")
      ),
      `Gene name` = colDef(name = "Gene name",
                           style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                           header = with_tooltip4("Gene name",
                                                  "Name of the motif")),
      Motif = colDef(name = "Motif name",
                     header = with_tooltip4("Motif name", "Name of the motif")),
      BBLS = colDef(name = "BBLS",
                    style = function(value) {
                      bar_style2(width = value / max(motifs$BBLS), fill = "#2c5e77", color = "#fff")
                    },
                    align = "left",
                    format = colFormat(digits = 3),
                    header = with_tooltip4("BBLS",
                                           "Bayesian Branch Length Score (BBLS) is assessment of the degree of evolutionary conservation")),
      `Distance(bp)` = colDef(name = "Distance (bp)",
                              style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                              header = with_tooltip4("Distance (bp)",
                                                     "Motif's distance from the gene (upstream)"))
    ),
    bordered = TRUE,
    sortable = TRUE)
  })
  
  
  output$motiftableUI<-renderUI({
    req(input$motifname)
    column(
      width = 12,
      reactableOutput("motiftbl2") %>% withSpinner(type = 8),
      br(),
      tagList("* For more information: ", a("http://motifmap.ics.uci.edu/", href = "http://motifmap.ics.uci.edu/",
                                            target="_blank"))
    )
  })
  
  ### Protein Atlas ----
  
  output$proAtlas<-renderReactable({
    reactable(protein_atlas, resizable = TRUE, filterable = TRUE, searchable = TRUE,
              defaultPageSize = 15, showPageSizeOptions = TRUE,
              columns = list(
                Gene_name = colDef(name = "Gene name", maxWidth = 110),
                Comment = colDef(html = TRUE),
                Keyword = colDef(html = TRUE, maxWidth = 110)
              ))
  })
  
  
  # Source Page ----
  
  output$sourcelisttable1<-renderText({
    
    paste(
      "<table style=\"font-size:15px;\">", "<col style=\"width: 20%;\"/>", "<col style=\"width: 80%;\"/>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "IntAct", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://www.ebi.ac.uk/intact/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://www.ebi.ac.uk/intact/", "</td>", "</tr>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "BioGRID", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://thebiogrid.org/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://thebiogrid.org/","</td>", "</tr>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "WormBase (Alliance)", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://www.alliancegenome.org/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://www.alliancegenome.org/", "</td>", "</tr>",
      
      "<table>")
  })
  
  output$sourcelisttable2<-renderText({
    
    paste(
      "<table style=\"font-size:15px;\">", "<col style=\"width: 20%;\"/>", "<col style=\"width: 80%;\"/>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "IntAct", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://www.ebi.ac.uk/intact/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://www.ebi.ac.uk/intact/", "</td>", "</tr>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "BioGRID", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://thebiogrid.org/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://thebiogrid.org/","</td>", "</tr>",
      
      "<tr>", "<td style=\"padding:0 0 10px 20px;\">", "WormBase (Alliance)", "</td>", "<td style=\"padding:0 0 10px 15px;\">", "<a href=", "https://www.alliancegenome.org/", "target=\"_blank\"", "rel=\"noopener noreferrer\"", "</a>", "https://www.alliancegenome.org/", "</td>", "</tr>",
      
      "<table>")
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
  
  ## Download page ----
  
  output$downloadtable2<-renderReactable({
    reactable(downloads2[1:4,], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Description = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$downloadtable3<-renderReactable({
    reactable(downloads2[5,], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Description = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$downloadtable4<-renderReactable({
    reactable(downloads2[6:8,], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Description = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$downloadtable5<-renderReactable({
    reactable(downloads2[9,], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Description = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$downloadtable6<-renderReactable({
    reactable(downloads2[10,], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Description = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$downloadtable1<-renderReactable({
    reactable(downloads, resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 100, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Organism = colDef(name = ""),
                Download = colDef(html = TRUE, name = "")
              ), height = 500
    )
  })
  
  ## Source page ----
  
  output$sourcetable<-renderReactable({
    reactable(sources[1:3,1:2], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Source = colDef(name = ""),
                Link = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$sourcetable1<-renderReactable({
    reactable(sources[4:7,1:2], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Source = colDef(name = ""),
                Link = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$sourcetable2<-renderReactable({
    reactable(sources[8,1:2], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Source = colDef(name = ""),
                Link = colDef(html = TRUE, name = "")
              )
    )
  })
  
  output$sourcetable3<-renderReactable({
    reactable(sources[9:13,1:2], resizable = FALSE, filterable = FALSE,
              searchable = FALSE, defaultPageSize = 20, showPageSizeOptions = FALSE,
              highlight = FALSE,
              columns = list(
                Source = colDef(name = ""),
                Link = colDef(html = TRUE, name = "")
              )
    )
  })
  
  waiter_hide()
}
