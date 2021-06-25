
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

  # Data

  # final_score_table<-reactive({
  #   db<-load_data_mysql()
  #   scores<-tbl(db, "new_scores") %>%
  #     collect()
  #   dbDisconnect(db)
  #   scores
  # })


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

  # List of genes having same synonym gene names
  # geneoption<-reactive({
  #   c(gene_synonyms2$Gene_name[which(toupper(gene_synonyms2$Gene_synonyms) %in% toupper(trimmedGname()))])
  # })

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
    #click("generalPage")
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

  observeEvent(input$`che-3`, {
    session$sendCustomMessage("geneName", "che-3")
    show("nvbr")
    show("inpt")
    updateNavbarPage(inputId = "nvbr", selected = "General info")

  })

  observeEvent(input$birc5b, {
    session$sendCustomMessage("geneName", "birc5b")
    # show("nvbr")
    # show("inpt")
    # updateNavbarPage(inputId = "nvbr", selected = "General info")
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
    # show("nvbr")
    # show("inpt")
    # updateNavbarPage(inputId = "nvbr", selected = "General info")
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
    final_score_table$Gene_name[selected2()]
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

  # output$buttonsui<-renderUI({
  #   if (!input$isMobile){
  #     absolutePanel(
  #       div(
  #         id = "tabButtons",
  #         column(
  #           width = 12,
  #           align = "center",
  #           offset = 1,
  #           br(),
  #           actionBttn("homePage", "Home",
  #                      icon = icon("home"),
  #                      style = "fill",
  #                      color = "default",
  #                      size = "sm"),
  #           actionBttn("generalPage", "General Information",
  #                      icon = icon("info"),
  #                      color = "primary",
  #                      style = "fill",
  #                      size = "sm"),
  #           actionBttn("proteinPage", "Protein interactions",
  #                      icon = img(src = "network2.png", height = "20px"),
  #                      color = "primary",
  #                      style = "fill",
  #                      size = "sm"),
  #           actionBttn("scclusterPage", "Single Cell Clusters",
  #                      icon = img(src = "tree.png", height = "20px"),
  #                      color = "primary",
  #                      style = "fill",
  #                      size = "sm"),
  #           br(),
  #           actionBttn("clusterPage", "Phylogenetic Analysis",
  #                      icon = img(src = "tree.png", height = "20px"),
  #                      color = "primary",
  #                      style = "fill",
  #                      size = "sm"),
  #           actionBttn("pubPage", "Publications",
  #                      icon = icon("book"),
  #                      color = "primary",
  #                      style = "fill",
  #                      size = "sm")
  #         )
  #       ),
  #       top = "70px",
  #       left = 0,
  #       right = 0,
  #       fixed = FALSE,
  #       style = "z-index: 10;"
  #     )
  #   }
  #   else {
  #     absolutePanel(
  #       div(
  #         id = "tabButtons",
  #         column(
  #           width = 12,
  #           align = "center",
  #           br(),br(),
  #           actionBttn("homePage", "Home",
  #                      icon = icon("home"),
  #                      style = "unite",
  #                      color = "default",
  #                      size = "sm"),
  #           actionBttn("generalPage", "General Information",
  #                      icon = icon("info"),
  #                      color = "primary",
  #                      style = "unite",
  #                      size = "sm"),
  #           actionBttn("proteinPage", "Protein interactions",
  #                      icon = img(src = "network2.png", height = "20px"),
  #                      color = "primary",
  #                      style = "unite",
  #                      size = "sm"),
  #           actionBttn("scclusterPage", "Single Cell Clusters",
  #                      icon = img(src = "tree.png", height = "20px"),
  #                      color = "primary",
  #                      style = "unite",
  #                      size = "sm"),
  #           actionBttn("clusterPage", "Phylogenetic Analysis",
  #                      icon = img(src = "tree.png", height = "20px"),
  #                      color = "primary",
  #                      style = "unite",
  #                      size = "sm"),
  #           actionBttn("pubPage", "Publications",
  #                      icon = icon("book"),
  #                      color = "primary",
  #                      style = "unite",
  #                      size = "sm")
  #         )
  #       ),
  #       top = "35px",
  #       left = 0,
  #       right = 0,
  #       fixed = FALSE,
  #       style = "z-index: 10;"
  #     )
  #   }
  # })

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

  # output$geneinfo<-renderText({
  #   sprintf("The data shown are for %s gene", genename())
  # })
  output$geneinfo<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  output$geneinfo1<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  output$geneinfo2<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  output$geneinfo3<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  output$geneinfo4<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  output$geneinfo5<-renderUI({
    HTML(paste("The data shown are for", '<b style="color:#edbc40">',genename(),'</b>', "gene"))
  })

  #### Gene info ----
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

  # inputseq<-reactive({
  #   final_score_table$Order[which(final_score_table$Gene_name == genename())]
  # })

  # inputseq1<-reactive({
  #   final_seq_table$Protein_interaction_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq2<-reactive({
  #   final_seq_table$Genetic_interaction_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq3<-reactive({
  #   final_seq_table$Single_cell_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq4<-reactive({
  #   final_seq_table$Cluster_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq5<-reactive({
  #   final_seq_table$Motif_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq6<-reactive({
  #   final_seq_table$Publication_score[which(final_seq_table$Gene_name == genename())]
  # })
  #
  # inputseq7<-reactive({
  #   final_seq_table$Protein_atlas_score[which(final_seq_table$Gene_name == genename())]
  # })

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


  # output$bargeneinfo<-renderUI({
  #   req(input$geneName)
  #   tagList(
  #     genebar("pb", inputseq(), "Overall percentile", 1165, 1930),
  #     genebar("pb1", inputseq1(), "Protein interaction", 2194, 8438),
  #     genebar("pb2", inputseq2(), "Genetic interaction", 285, 317),
  #     genebar("pb3", inputseq3(), "Single cell", 1868, 2923),
  #     genebar("pb4", inputseq4(), "Cluster", 4845, 7759),
  #     genebar("pb5", inputseq5(), "Motif", 702, 1878),
  #     genebar("pb6", inputseq6(), "Publication", 7302, 11690),
  #     genebar("pb7", inputseq7(), "Protein Atlas", 651, 1016)
  #   )
  # })

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
    #req(networkdata())
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
    #a<-data.frame(Publication = unique(publications$Publication[which(toupper(publications$Gene_name) %in% toupper(genename()))]))
    b<-merge(pub, publ, by = "Publication")
    b$Link <- paste0("<a href='",b$Link,"' target='_blank'>",b$Paper,"</a>")
    b
  })

  output$pubtable<-renderReactable({
    reactable(pubdata()[,c(-2,-7)], resizable = TRUE, filterable = TRUE,
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
    if (length(pubdata()[[1]]) > 0){
      reactableOutput("pubtable")
    }
    else {
      h4(textOutput("puberror"))
    }
  })

  pub_mat<-reactive({
    db<-load_data_mysql()
    pub<-tbl(db, "pub_mat") %>%
      filter(Gene_name %in% local(genename())) %>%
      collect()
    pub2<-tbl(db, "pub_mat") %>%
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
                          header = div(tags$style("display: flex; flexDirection: column; justifyContent: center; text-align: center;"),
                                       "Consensus sequence")
                          ),
      Motif = colDef(name = "Motif",
                     style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                     header = with_tooltip3("Motif",
                                           "Name of the motif")),
      `Motif ID` = colDef(name = "Motif ID"),
      BBLS = colDef(name = "BBLS",
                    style = function(value) {
                      bar_style(width = value / max(motifs$BBLS), fill = "#2c5e77", color = "#fff")
                    },
                    align = "left",
                    format = colFormat(digits = 3),
                    header = with_tooltip3("BBLS",
                                          "BBLS () is conservation score.")),
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
    if (as.numeric(input$GetScreenWidth) >= 1600){
      div(
        style = "position: relative",
        column(
          id = "colcolcol",
          width = 10,
          #align = "center",
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
            withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "630px"), type = 8),
            br(),br(),br()
          )
        )
      )
    }
    else {
      div(
        style = "position: relative",
        column(
          id = "colcolcol",
          width = 12,
          #align = "center",
          #offset = 1,
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
            withSpinner(iheatmaprOutput("heatmapcluster", width = "100%", height = "630px"), type = 8),
            br(),br(),br()
          )
        )
      )
    }
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
      #align = "center",
      box(
        title = paste("Genes in cluster", inputclusternamenumber()),
        solidHeader = TRUE,
        status = "success",
        width = 12,
        withSpinner(reactableOutput("hclustertable"), type = 8)
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

  # feature.names<-reactive({
  #   input$scsource2
  #   if(input$scsource2 == "Cao et al(2017) - C. elegans"){
  #     x<-isolate(orthology$Gene1Symbol[orthology$Gene2Symbol == genename() & orthology$Gene1SpeciesName == "Caenorhabditis elegans"])
  #   }
  #   else {x<-isolate(genename())}
  # })
  #
  # source.list2<-reactive({
  #   sc.paper.list$data[sc.paper.list$paper == input$scsource2]
  # })
  #
  # output$scumapgeneral2_binding<-renderPlot({
  #   req(input$scsource2)
  #   #input$geneName
  #   if(input$scsource2 == "Cao et al(2017) - C. elegans"){
  #     if(length(feature.names())>1){
  #       req(input$celeinput)
  #       ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
  #         geom_point(size = 0.05) +
  #         #guides(colour = guide_legend(override.aes = list(size=2))) +
  #         ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
  #         theme_classic() +
  #         theme(legend.position = "none")
  #     }
  #     else{
  #       ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
  #         geom_point(size = 0.05) +
  #         #guides(colour = guide_legend(override.aes = list(size=2))) +
  #         ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
  #         theme_classic() +
  #         theme(legend.position = "none")
  #     }
  #   }
  #   else if(input$scsource2 == "Habermann et al(2020) - Lung (human)"){
  #     DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE, repel = TRUE, raster = FALSE) +
  #     NoLegend()
  #   }
  #   else{
  #     DimPlot(object = eval(parse(text = source.list2())), reduction = "umap", label = TRUE)
  #   }
  # })
  #
  #  celeinput1<-reactive({
  #    input$celeinput
  #  })
  #
  # scmapgeneplot<-reactive({
  #   req(input$scsource2)
  #   input$scsource2
  #   if(input$scsource2 == "Cao et al(2017) - C. elegans"){
  #     if(length(feature.names())>1){
  #       req(input$celeinput)
  #       p<-isolate(plot.expr(cds, celeinput1(), cell_size = 0.05) + theme(legend.position = "right"))
  #     }
  #     else{
  #       p<-isolate(plot.expr(cds, feature.names(), cell_size = 0.05) + theme(legend.position = "right"))
  #     }
  #   }
  #   else {
  #     p<-isolate(FeaturePlot(object = eval(parse(text = source.list2())), features = feature.names(), pt.size = 0.05))
  #     #HoverLocator(plot = plot, information = FetchData(eval(parse(text = source.list2())), vars = c("ident","nFeature_RNA","nCount_RNA")))
  #   }
  #   p
  # })
  #
  # output$scmapgene<-renderPlot({
  #   input$scsource2
  #   p<-isolate(scmapgeneplot())
  #   p
  # })
  #
  # output$vlngene<-renderPlot({
  #   req(input$scsource2)
  #   input$scsource2
  #   if(input$scsource2 == "Cao et al(2017) - C. elegans" & !is.null(input$celeinput)){
  #     req(input$celeinput)
  #     p<-isolate(VlnPlot(eval(parse(text = source.list2())), features = input$celeinput))
  #   }
  #   else{
  #     p<-isolate(VlnPlot(eval(parse(text = source.list2())), features = feature.names()))
  #   }
  #   p
  # })
  #
  # dfforvln<-reactive({
  #   if(length(feature.names())>1){
  #     row.names(subset(cele_genes, symbol == input$celeinput))
  #   }
  #   else{
  #     row.names(subset(cele_genes, symbol == feature.names()))
  #   }
  # })
  #
  # output$vlngene2<-renderPlot({
  #   plot_genes_jitter(cds[dfforvln(),], grouping = "cell.type", color_by = "cell.type", nrow= 1,
  #                     ncol = NULL, plot_trend = TRUE) +
  #     theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5), legend.position = "none")
  # })
  #
  #
  # output$scInputUI2<-renderUI({
  #   input$geneName
  #   fluidRow(
  #     div(
  #       id = "scselectUI2",
  #       column(
  #         width = 6,
  #         align = "center",
  #         offset = 3,
  #         h3(paste0("Select a single cell RNA-seq data to visualize cell groups and expression profile of ",
  #                   genename(),
  #                   " gene")),
  #         br(),
  #         pickerInput(
  #           inputId = "scsource2",
  #           label = "Select source of scRNA-seq data",
  #           choices = list(
  #             "Carraro et al(2021) - Lung (human)",
  #             "Reyfman et al(2018) - Lung (human)",
  #             "Habermann et al(2020) - Lung (human)",
  #             "Cao et al(2017) - C. elegans"
  #           ),
  #           selected = NULL,
  #           multiple = TRUE,
  #           options = pickerOptions(maxOptions = 1)
  #         ),
  #         uiOutput("cele_options")
  #       )
  #     )
  #   )
  # })
  #
  # observeEvent(input$nvbr, {
  #   if(input$nvbr == "Home"){
  #     hide("scUIdiv2")
  #     reset("scsource2")
  #     reset("scmapgene")
  #     reset("singlegeneexp")
  #     reset("vlngene")
  #     updatePickerInput(session = session, "scsource2", selected = NULL)
  #     click("geneName_reset")
  #   }
  # })
  #
  #
  # output$cele_options<-renderUI({
  #   req(input$scsource2)
  #   if(length(feature.names())>1){
  #     div(
  #       pickerInput(
  #         inputId = "celeinput",
  #         label = "Select a C. elegans gene",
  #         choices = feature.names(),
  #         selected = NULL,
  #         multiple = TRUE,
  #         options = pickerOptions(maxOptions = 1)
  #       )
  #     )
  #   }
  #   else {}
  # })
  #
  # output$singlegeneexp<-renderUI({
  #   input$scsource2
  #   if(input$scsource2 == "Cao et al(2017) - C. elegans"){
  #     if(length(feature.names()) > 1){
  #       req(input$celeinput)
  #     }
  #     else if(length(feature.names()) == 0){
  #       h4(paste0("There is no ortholog of ", genename(), " gene in C. elegans"))
  #     }
  #     p<-isolate(plotOutput("scmapgene", height = "600px") %>% withSpinner(type = 8))
  #   }
  #   else {
  #     p<-isolate(plotOutput("scmapgene", height = "600px") %>% withSpinner(type = 8))
  #     #bsTooltip("scgeneinput", "Select a gene to display its expression across cells", placement = "top")
  #   }
  # })
  #
  # output$scUI2<-renderUI({
  #   req(input$scsource2)
  #   if (is.null(input$scsource2)){
  #
  #   }
  #   else if(input$scsource2 == "Cao et al(2017) - C. elegans"){
  #     if(length(feature.names())>1 & is.null(input$celeinput)){
  #
  #     }
  #     else {
  #       div(
  #         id = "scUIdiv2",
  #         fluidRow(
  #           box(
  #             width = 12,
  #             column(
  #               width = 6,
  #               plotOutput("scumapgeneral2_binding", height = "600px") %>% withSpinner(type = 8)
  #               #bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
  #             ),
  #             column(
  #               width = 6,
  #               uiOutput("singlegeneexp")
  #             )
  #           )
  #         ),
  #         fluidRow(
  #           box(
  #             width = 12,
  #             column(
  #               width = 12,
  #               plotOutput("vlngene2", height = "500px") %>% withSpinner(type = 8)
  #             )
  #           )
  #         )
  #       )
  #     }
  #   }
  #
  #   else{
  #     div(
  #       id = "scUIdiv2",
  #       fluidRow(
  #         box(
  #           width = 12,
  #           column(
  #             width = 6,
  #             plotOutput("scumapgeneral2_binding") %>% withSpinner(type = 8)
  #             #bsTooltip("scsource", "Select a source to visualize the cells", placement = "top"),
  #           ),
  #           column(
  #             width = 6,
  #             uiOutput("singlegeneexp")
  #           )
  #         )
  #       ),
  #       fluidRow(
  #         box(
  #           width = 12,
  #           column(
  #             width = 12,
  #             plotOutput("vlngene") %>% withSpinner(type = 8)
  #           )
  #         )
  #       )
  #     )
  #   }
  # })









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
                # Norm_total_score = colDef(header = with_tooltip("Normalized score",
                #                                                 "Total normalized scores"),
                #                           format = colFormat(digits = 3)),
                # Weighted_total_scores = colDef(header = with_tooltip("Weighted score",
                #                                                      "Total weighted scores. Weights are based on the success rate of finding ciliary genes."),
                #                                format = colFormat(digits = 3)),
                Order = colDef(header = with_tooltip("Order",
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
    req(input$clusternumber)
        a<-as.matrix(nscores2[which(nscores2$cluster_number == input$clusternumber),2:73])
        rownames(a)<-nscores2$Gene_name[which(nscores2$cluster_number == input$clusternumber)]
        a
    })

  inputclusternumbertable<-reactive({
    req(input$clusternumber)
    a<-data.frame('Gene name' = nscores2[which(nscores2$cluster_number == input$clusternumber),1])
    a$Score<-final_score_table$Mean_score[match(a[[1]], final_score_table$Gene_name)]
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
  })

  output$heatmapclusternumber<-renderIheatmap({
    heatmapclusternumberR()
  })

  output$heatmapclusternumberUI<-renderUI({
    if (as.numeric(input$GetScreenWidth) >= 1600){
      column(
        width = 10,
        offset = 1,
        box(
          title = "Phylogenetic analysis",
          solidHeader = TRUE,
          status = "success",
          width = 12,
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
              withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), type = 8)
            ),
            column(
              width = 12,
              align = "left",
              div(
                id = "button3",
                style = "left: 10ex;",
                #dropdown(
                downloadButton(outputId = "hmap3", label = "Download")
                #   size = "xm",
                #   icon = icon("download", class = "opt"),
                #   up = TRUE
                # )
              )
            )
          )
        )
      )
    }
    else {
      column(
        width = 12,
        #offset = 1,
        box(
          title = "Phylogenetic analysis",
          solidHeader = TRUE,
          status = "success",
          width = 12,
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
              withSpinner(iheatmaprOutput("heatmapclusternumber", height = "600px"), type = 8)
            ),
            column(
              width = 12,
              align = "left",
              div(
                id = "button3",
                style = "left: 10ex;",
                #dropdown(
                downloadButton(outputId = "hmap3", label = "Download")
                #   size = "xm",
                #   icon = icon("download", class = "opt"),
                #   up = TRUE
                # )
              )
            )
          )
        )
      )
    }
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
  #
  # output$schclusternumbertable<-renderReactable({
  #   reactable(r_scclusternumbertable(), resizable = TRUE, filterable = TRUE,
  #             columns = list(Score = colDef(format = colFormat(digits = 3)),
  #                            Gene_name = colDef(name = "Gene name")),
  #             defaultPageSize = 10, showPageSizeOptions = TRUE,
  #             highlight = TRUE)
  # })
  #
  # scheatmapclusternumberR<-reactive({
  #   main_heatmap(r_scclusternumber2(), layout = list(paper_bgcolor='transparent'),
  #                tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #     #add_row_labels(size = 0.03,font = list(family = c("open_sansregular"), size = 7))%>%
  #     add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                    tickvals = c(1:length(colnames(r_scclusternumber2()))))%>%
  #     add_col_annotation(annotation=anot_sc, side="top", size = 0.1) %>%
  #     modify_layout(list(margin = list(l = 80)))
  # })
  #
  # scheatmapgenenumberR<-reactive({
  #   main_heatmap(r_scgenenumber(), layout = list(paper_bgcolor='transparent'),
  #                tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #     #add_row_labels(size = 0.03, font = list(family = c("open_sansregular"), size = 12))%>%
  #     add_col_labels(size = 1, font = list(family = c("open_sansregular"), size = 12), textangle=90,
  #                    tickvals = c(1:length(colnames(r_scgenenumber()))))%>%
  #     add_col_annotation(annotation=anot_sc, side="top", size = 0.1) %>%
  #     modify_layout(list(margin = list(l = 80)))
  # })
  #
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
  # output$scInputUI<-renderUI({
  #   fluidRow(
  #     div(
  #       id = "scselectUI",
  #       column(
  #         width = 6,
  #         align = "center",
  #         offset = 3,
  #         h3("Select a single cell RNA-seq data to visualize cell groups and gene expressions"),
  #         br(),
  #         pickerInput(
  #           inputId = "scsource",
  #           label = "Select source of scRNA-seq data",
  #           choices = list(
  #             "Carraro et al(2021) - Lung (human)",
  #             "Reyfman et al(2018) - Lung (human)",
  #             "Habermann et al(2020) - Lung (human)",
  #             "Cao et al(2017) - C. elegans"
  #           ),
  #           selected = NULL,
  #           multiple = TRUE,
  #           options = pickerOptions(maxOptions = 1)
  #         )
  #       )
  #     )
  #   )
  # })
  #
  # output$scUI<-renderUI({
  #   if (is.null(input$scsource)){
  #
  #   }
  #   else {
  #     div(
  #       id = "scUIdiv",
  #       # fluidRow(
  #       #   column(
  #       #     width = 6,
  #       #     offset = 6,
  #       #     uiOutput("scgeneinput"),
  #       #     uiOutput("scgenebutton")
  #       #   )
  #       # ),
  #       fluidRow(
  #         box(
  #           width = 12,
  #           column(
  #             width = 6,
  #             plotOutput("scumapgeneral",height = "600px") %>% withSpinner(type = 8),
  #             bsTooltip("scsource", "Select a source to visualize the cells", placement = "top")
  #           ),
  #           column(
  #             width = 6,
  #             fluidRow(
  #               uiOutput("message"),
  #               column(
  #                 width = 6,
  #                 uiOutput("scgeneinput")
  #               ),
  #               column(
  #                 width = 3,
  #                 br(),
  #                 uiOutput("scgenebutton")
  #               )
  #             ),
  #             fluidRow(
  #               column(
  #                 width = 12,
  #                 br(),
  #                 plotOutput("dotgene", height = "600px") %>% withSpinner(type = 8)
  #               )
  #             )
  #           )
  #           # column(
  #           #   width = 6,
  #           #   plotOutput("heatmapgene") %>% withSpinner(type = 8),
  #           # )
  #         )
  #       ),
  #       fluidRow(
  #         column(
  #           width = 8,
  #           align = "center",
  #           offset = 2,
  #           br(),br(),
  #           h4("Select a cluster to explore differentially expressed genes"),
  #           box(
  #             width = 12,
  #             uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
  #             reactableOutput("scmaptable"),
  #             bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
  #           )
  #         )
  #       )
  #     )
  #   }
  # })
  #
  # # output$scUI2<-renderUI({
  # #   fluidRow(
  # #     column(
  # #       width = 9,
  # #       uiOutput("sccelltypeinput") %>% withSpinner(type = 8),
  # #       reactableOutput("scmaptable"),
  # #       bsTooltip("sccelltypeinput", "Select a group to list genes differentially expressed in that group", placement = "top")
  # #     )
  # #   )
  # # })
  #
  # # observeEvent(input$scsource,{
  # #   if (is.null(input$scsource)){
  # #     hide("scUIdiv")
  # #   }
  # #   else {
  # #     show("scUIdiv")
  # #   }
  # # })
  #
  #
  # # Server #
  # output$message<-renderUI({
  #   h4("Select a group of genes to visualize expressions in a dot plot")
  # })
  #
  # source.list<-reactive({
  #   sc.paper.list$data[sc.paper.list$paper == input$scsource]
  # })
  #
  # output$scumapgeneral<-renderPlot({
  #   req(input$scsource)
  #   if(input$scsource == "Cao et al(2017) - C. elegans"){
  #     ggplot(cele_data, aes(tsne_1, tsne_2, colour = cell.type), label = TRUE) +
  #       geom_point(size = 0.05) +
  #       guides(colour = guide_legend(override.aes = list(size=2))) +
  #       ggrepel::geom_text_repel(data = label.df_2, aes(label = label), colour = "black", size = 3.2) +
  #       theme_classic() +
  #       theme(legend.position = "none")
  #   }
  #   else if(input$scsource == "Habermann et al(2020) - Lung (human)"){
  #     DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE, repel = TRUE, raster = FALSE) +
  #     NoLegend()
  #   }
  #   else{
  #     DimPlot(object = eval(parse(text = source.list())), reduction = "umap", label = TRUE)
  #   }
  # })
  #
  # # output$scgeneralmaps<-renderUI({
  # #   if (input$scsource == "Cao et al(2017) - C. elegans"){
  # #
  # #   }
  # # })
  #
  # # output$scgeneinput<-renderUI({
  # #   req(input$scsource)
  # #   pickerInput(
  # #     inputId = "scgene",
  # #     label = "Select a gene",
  # #     choices = rownames(eval(parse(text = source.list()))),
  # #     selected = NULL,
  # #     multiple = TRUE,
  # #     options = pickerOptions(maxOptions = 1,
  # #                             liveSearch = TRUE)
  # #   )
  # # })
  #
  # output$scgeneinput<-renderUI({
  #   req(input$scsource)
  #   selectInput(
  #     "scgene",
  #     "Select multiple genes",
  #     multiple = TRUE,
  #     choices = NULL
  #   )
  #   # pickerInput(
  #   #   inputId = "scgene",
  #   #   label = "Select multiple genes",
  #   #   choices = rownames(eval(parse(text = source.list()))),
  #   #   selected = NULL,
  #   #   multiple = TRUE,
  #   #   options = pickerOptions(liveSearch = TRUE)
  #   # )
  # })
  #
  # # selectchoices<-reactive({
  # #   if (!is.null(input$scsource)){
  # #     x<-sc.paper.list$data[sc.paper.list$paper == input$scsource]
  # #     as.character(rownames(x))
  # #   }
  # # })
  #
  # observeEvent(input$scsource, {
  #   if (input$scsource == "Carraro et al(2021) - Lung (human)"){
  #     delay(500, updateSelectizeInput(session, "scgene", choices = lung_names, server = TRUE))
  #   }
  #   else if(input$scsource == "Reyfman et al(2018) - Lung (human)"){
  #     delay(500, updateSelectizeInput(session, "scgene", choices = reyfman_names, server = TRUE))
  #   }
  #   else if(input$scsource == "Habermann et al(2020) - Lung (human)"){
  #     delay(500, updateSelectizeInput(session, "scgene", choices = habermann_names, server = TRUE))
  #   }
  #   else if(input$scsource == "Cao et al(2017) - C. elegans"){
  #     delay(500, updateSelectizeInput(session, "scgene", choices = cele_names, server = TRUE))
  #   }
  # })
  #
  # # observe({
  # #   if (!is.null(input$scsource)){
  # #     if (input$scsource == "Carraro et al(2021) - Lung"){
  # #       input_choice<-lung_names
  # #     }
  # #     else if(input$scsource == "Reyfman et al(2018) - Lung"){
  # #       input_choice<-reyfman_names
  # #     }
  # #     else{
  # #       input_choice<-cele_names
  # #     }
  # #   }
  # #
  # #   #input_placeholder<-isolate(input$scgene)
  # #   isolate(
  # #     updateSelectizeInput(session, "scgene", choices = input_choice, server = TRUE)
  # #   )
  # # })
  #
  # # observe({
  # #   input$scgene
  # #   isolate(updateSelectizeInput(session, "scgene", choices = lung_names, server = TRUE))
  # # })
  #
  # output$scgenebutton<-renderUI({
  #   req(input$scsource)
  #   actionButton("scbttn","Draw",icon = icon("thumbs-up"))
  # })
  #
  # # output$scgeneinput<-renderUI({
  # #   req(input$scsource)
  # #   if (input$scsource == "Carraro et al(2021) - Lung"){
  # #     autocomplete_input(
  # #       id = "scgene",
  # #       label = "Select a gene",
  # #       options = rownames(lung),
  # #       max_options = 50
  # #     )
  # #   }
  # # })
  #
  # # observeEvent(input$scsource, {
  # #   update_autocomplete_input(
  # #     session,
  # #     "scgene",
  # #     options = rownames(lung)
  # #   )
  # # })
  #
  # #updateSelectizeInput(session, "scgene", choices = rownames(lung), server = TRUE)
  #
  # # output$scmapgene<-renderPlot({
  # #   req(input$scgene)
  # #   plot<-FeaturePlot(object = eval(parse(text = source.list())), features = input$scgene)
  # #   HoverLocator(plot = plot, information = FetchData(reyfmans.reduced, vars = c("ident","nFeature_RNA","nCount_RNA")))
  # # })
  #
  # scgene1<-eventReactive(input$scbttn, {
  #   input$scbttn
  #   isolate(input$scgene)
  # })
  #
  # output$dotgene<-renderPlot({
  #   if(input$scsource == "Cao et al(2017) - C. elegans"){
  #     DotPlot(eval(parse(text = source.list())), features = scgene1()) + RotatedAxis() +
  #       theme(axis.title.y = element_text(size=1))
  #   }
  #   else {
  #     DotPlot(eval(parse(text = source.list())), features = scgene1()) + RotatedAxis()
  #   }
  # })
  #
  # # output$heatmapgene<-renderPlot({
  # #   req(input$scgene)
  # #   #req(input$scbttn)
  # #   input$scbttn
  # #   DoHeatmap(subset(eval(parse(text = source.list())), downsample = 100), features = input$scgene, size = 3)
  # # })
  #
  # output$sccelltypeinput<-renderUI({
  #   req(input$scsource)
  #   pickerInput(
  #     inputId = "sccelltypes",
  #     label = "Select cluster to get gene list",
  #     choices = c(levels(eval(parse(text = source.list())))),
  #     selected = NULL,
  #     multiple = TRUE,
  #     options = pickerOptions(maxOptions = 1),
  #     width = "80%"
  #   )
  #
  # })
  #
  # diff_list<-reactive({
  #   a<-levels(eval(parse(text = source.list())))
  #   i<-which(a == input$sccelltypes)
  #   if (source.list() == "lung"){
  #     lung_markers[[i]]
  #   }
  #   else if (source.list() == "reyfman"){
  #     reyfman_markers[reyfman_markers$cluster == input$sccelltypes,]
  #   }
  #   else if (source.list() == "habermann"){
  #     habermann_markers[habermann_markers$cluster == input$sccelltypes,]
  #   }
  # })
  #
  # output$sometext<-renderText({
  #
  #   a<-levels(eval(parse(text = source.list())))
  #   i<-which(a == input$sccelltypes)
  #   paste("i is:", i)
  # })
  #
  # output$scmaptable<-renderReactable({
  #   req(input$sccelltypes)
  #   reactable(
  #     diff_list(), resizable = TRUE, filterable = TRUE,
  #     searchable = TRUE, defaultPageSize = 10, showPageSizeOptions = TRUE,
  #     highlight = TRUE
  #   )
  # })
  #
  # # output$scmaptable<-renderDT({
  # #   #req(input$sccelltypes)
  # #   as.data.table(diff_list())
  # # })
  #
  # # tippy
  #
  # output$tippy1<-renderUI({
  #   column(
  #     width = 1,
  #     br(),br(),
  #     tippy(
  #       bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
  #       "<span style='font-size:15px;'>Select a source to visualize the cells!<span>",
  #       placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
  #     )
  #   )
  # })
  # output$tippy2<-renderUI({
  #   req(input$scsource)
  #   column(
  #     width = 1,
  #     br(),br(),
  #     tippy(
  #       bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
  #       "<span style='font-size:15px;'>Select a gene to display its expression across cells.<span>",
  #       placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
  #     )
  #   )
  # })
  # output$tippy3<-renderUI({
  #   req(input$scsource)
  #   column(
  #     width = 1,
  #     br(),br(),
  #     tippy(
  #       bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "extra-small"),
  #       "<span style='font-size:15px;'>Select a group to list genes differentially expressed in that group.<span>",
  #       placement = "top", animation = "scale", arrow = TRUE, theme = "blue"
  #     )
  #   )
  # })


  ### Publications ----
  # pubheatmapx<-reactive({
  #   req(input$pubgene)
  #   main_heatmap(as.matrix(pubgenelist_mat()), layout = list(paper_bgcolor='transparent'),
  #                tooltip = setup_tooltip_options(prepend_row = "Gene: ", prepend_col = "Cell type: "))%>%
  #     add_col_labels(size = 0.46,font = list(family = c("open_sansregular"), size = 12), textangle=90)%>%
  #     modify_layout(list(margin = list(b = 200)))
  #
  # })
  #
  # output$pubgeneralheatmap<-renderIheatmap({
  #   pubheatmapx()
  # })

  output$selectgene<-renderText("Please select a gene to visualize associated publications!")

  # output$pubgeneralheatmapUi<-renderUI({
  #   req(input$pubgene)
  #   if(input$pubgene == ""){
  #     textOutput("selectgene")
  #   }
  #   else {
  #     withSpinner(iheatmaprOutput("pubgeneralheatmap"), type = 8)
  #   }
  # })

  # pubgenelist_mat<-reactive({
  #   if(input$pubgene != "All"){
  #     pub_genes[pub_genes$Gene_name == input$pubgene,-1]
  #   }
  # })
  #
  # output$pubpickeroutput<-renderUI({
  #   fluidRow(
  #     id = "pubpicker",
  #     column(
  #       width = 3,
  #       pickerInput(
  #         inputId = "pubgene",
  #         label = "Select a gene",
  #         choices = list(
  #           "Gene name" = c("", pub_genes$Gene_name)
  #         ),
  #         selected = "",
  #         options=pickerOptions(liveSearch=T)
  #       )
  #     ),
  #     column(
  #       width = 1,
  #       br(),
  #       tippy(bsButton("tooltipbuttonpub", label = "", icon = icon("question"), style = "info", size = "big"),
  #             "<span style='font-size:15px;'>Please select a gene to visualize associated publications!<span>",
  #             placement = "right", animation = "scale", arrow = TRUE, theme = "blue")
  #     )
  #   )
  # })

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
                              width = 300)
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
    #xc4[xc4$Publication == input$pubpub,c(1,5,6)]
  })

  selected5 <- reactive(getReactableState("pubselecttable2", "selected"))

  pubselecttable_selection<-reactive({
    pub_list()[[2]][selected5()]
  })

  output$pubselecttable<-renderReactable({
    #req(input$pubpub)
    reactable(gene_pub_list(), resizable = TRUE, filterable = FALSE,
              searchable = TRUE, defaultPageSize = 20, showPageSizeOptions = TRUE,
              highlight = TRUE, height = 500
    )
  })

  output$publicationUI1<-renderUI({
    if (is.null(selected5())){
      column(
        width = 6,
        box(
          title = sprintf("Gene list for %s", pubselecttable_selection()),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          # pickerInput(
          #   inputId = "pubpub",
          #   label = "Select a publication",
          #   choices = list(
          #     "Publication" = c("", unique(publications$Publication))
          #   ),
          #   selected = "",
          #   options=pickerOptions(liveSearch=T)
          # ),
          #h3(sprintf("Gene list for %s", pubselecttable_selection())),
          h4("Select a publication from the table (left) to see the published gene list")
        )
      )
    }
    else {
      column(
        width = 6,
        box(
          title = sprintf("Gene list for %s", pubselecttable_selection()),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          # pickerInput(
          #   inputId = "pubpub",
          #   label = "Select a publication",
          #   choices = list(
          #     "Publication" = c("", unique(publications$Publication))
          #   ),
          #   selected = "",
          #   options=pickerOptions(liveSearch=T)
          # ),
          #h3(sprintf("Gene list for %s", pubselecttable_selection())),
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
          highchartOutput("pubchart") %>% withSpinner(type = 8)
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
          highchartOutput("pubchart") %>% withSpinner(type = 8)
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
      `Motif ID` = colDef(name = "Motif id"
      ),
      `Gene name` = colDef(name = "Gene name",
                     style = "display: flex; flexDirection: column; justifyContent: center; text-align: center;",
                     header = with_tooltip4("Gene name",
                                            "Name of the motif")),
      Motif = colDef(name = "Motif name"),
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

  downloadUI<-renderUI({

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
    reactable(sources[9:12,1:2], resizable = FALSE, filterable = FALSE,
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
