drugInformationServer <- function(id, drug_choices, drug_info_data, connect_to_db) {
  moduleServer(id, function(input, output, session) {

    initial_run_done <- reactiveVal(FALSE)
    
    full_drug_data_for_download <- reactiveVal(NULL)

    observeEvent(input$help_genes, {
      showModal(modalDialog(
        title = "Column Definitions",
        div(
          style = "max-height: 60vh; overflow-y: auto; padding: 20px;",
          h4("Field Explanations", style = "color: #4158D0; margin-bottom: 20px;"),
          
          div(style = "margin-bottom: 15px;",
              strong("Standard Name:", style = "color: #4158D0;"),
              " Drug names standardized using PubChem database nomenclature"
          ),
          div(style = "margin-bottom: 15px;",
              strong("Genes:", style = "color: #4158D0;"),
              " Gene symbols with high consistency (Top 25%)"
          ),
          div(style = "margin-bottom: 15px;",
              strong("GMCS:", style = "color: #4158D0;"),
              " Gene Mean Consistency Score - measures gene expression consistency across datasets (higher = more consistent)"
          ),
          div(style = "margin-bottom: 15px;",
              strong("ATC Level 3:", style = "color: #4158D0;"),
              " Anatomical Therapeutic Chemical classification codes for therapeutic subgroups"
          ),
          div(style = "margin-bottom: 15px;",
              strong("Drug Name (ATC L3):", style = "color: #4158D0;"),
              " Human-readable therapeutic category descriptions"
          ),
          div(style = "margin-bottom: 15px;",
              strong("PubMed Count:", style = "color: #4158D0;"),
              " Number of publications linking the drug and gene (higher = stronger literature evidence)"
          )
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    perform_drug_analysis <- function(selected_drug) {
      req(selected_drug)
      show_modal_spinner(spin = "flower", color = "#2c3e50", text = paste("Loading data for", selected_drug, "..."))
      
      db <- connect_to_db("drug_gene")
      
      query <- sprintf("
        SELECT standard_name, genes, GMCS, Level_3_Code, Drug_Name_3, pubmed_count 
        FROM merged_results_grouped 
        WHERE standard_name = '%s'", selected_drug)
      
      merged_results_grouped <- dbGetQuery(db, query)
      df <- merged_results_grouped
      
      if (nrow(df) > 0) {
        df$pubmed_count_numeric <- df$pubmed_count
        
        df <- df[order(-df$pubmed_count_numeric), ]
        
        df$standard_name_link <- paste0(
          '<a href="https://pubchem.ncbi.nlm.nih.gov/#query=', URLencode(df$standard_name), '" target="_blank">', df$standard_name, '</a>'
        )
        df$genes_link <- paste0(
          '<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=', df$genes, '" target="_blank">', df$genes, '</a>'
        )
        search_terms <- paste0(df$standard_name, ' AND ', df$genes)
        pubmed_urls  <- paste0('https://pubmed.ncbi.nlm.nih.gov/?term=', URLencode(search_terms, reserved = TRUE))

        df$pubmed_link <- paste0('<a href="', pubmed_urls, '" target="_blank">', df$pubmed_count, '</a>')
        create_atc_links <- function(codes) {
          if (is.na(codes) || codes == "") return("N/A")
          codes_list <- unlist(strsplit(codes, ","))
          codes_list <- trimws(codes_list)
          links <- paste0('<a href="https://www.whocc.no/atc_ddd_index/?code=', codes_list, '" target="_blank">', codes_list, '</a>')
          paste(links, collapse = ", ")
        }
        df$Level_3_Code_link <- sapply(df$Level_3_Code, create_atc_links)
        df$GMCS <- as.numeric(df$GMCS)
        df_display <- df[, c("standard_name_link", "genes_link", "GMCS", "Level_3_Code_link", "Drug_Name_3", "pubmed_link")]
        colnames(df_display) <- c("Standard Name", "Genes", "GMCS", "ATC Level 3", "Drug Name (ATC L3)", "PubMed Count")
        
        df_download <- df[, c("standard_name", "genes", "GMCS", "Level_3_Code", "Drug_Name_3", "pubmed_count")]
        colnames(df_download) <- c("Standard Name", "Genes", "GMCS", "ATC Level 3", "Drug Name (ATC L3)", "PubMed Count")
        full_drug_data_for_download(df_download)
        
        output$drug_gene_table <- renderDT({
          datatable(
            df_display, escape = FALSE, filter = "top", rownames = FALSE, 
            options = list(
              pageLength = 10, lengthMenu = c(10, 20, 50, 100), autoWidth = TRUE, dom = 'Blfrtip',
              buttons = list(list(extend = 'collection', 
                                  buttons = list(
                                    list(extend = 'csv', 
                                         text = 'Download CSV (Full Data)',
                                         action = DT::JS("function ( e, dt, node, config ) {
                                           Shiny.setInputValue('drugInfo-download_drug_csv', true, {priority: 'event'});
                                         }")),
                                    list(extend = 'excel', 
                                         text = 'Download Excel (Full Data)',
                                         action = DT::JS("function ( e, dt, node, config ) {
                                           Shiny.setInputValue('drugInfo-download_drug_excel', true, {priority: 'event'});
                                         }"))
                                  ), 
                                  text = 'Download Full Data')),
              lengthChange = TRUE, scrollX = TRUE, scrollY = "450px", scrollCollapse = TRUE,
              columnDefs = list(
                list(orderable = FALSE, targets = 5),
                list(className = 'dt-center', targets = "_all")
              )
            ), extensions = 'Buttons', selection = "none"
          ) %>% formatRound(columns = c("GMCS"), digits = 3)
        }, server = TRUE)
        
      } else {
        output$drug_gene_table <- renderDT({
          datatable(data.frame(Message = paste("No associated gene data found for", selected_drug)), options = list(dom = 't'))
        }, server = TRUE)
      }
      
      drug <- drug_info_data[drug_info_data$Title == selected_drug, ]
      if (nrow(drug) == 0) {
        output$drugInfo <- renderUI({
          div(style="text-align: center; padding: 20px; color: grey;", h4(paste("Drug information not found for", selected_drug)))
        })
      } else {
        drug <- drug[1, ]
        local_img_path <- file.path("www", "CID_images", paste0(drug$CID, ".png"))
        img_src <- if (!file.exists(local_img_path)) "CID_images/default.png" else file.path("CID_images", paste0(drug$CID, ".png"))
        
        output$drugInfo <- renderUI({
          tags$table(
            class = "layui-table mt-0", style = "margin-bottom: 0;",
            tags$colgroup( tags$col(width = "26%"), tags$col(width = "20%"), tags$col(width = "") ),
            tags$tbody(
              tags$tr( tags$td(rowspan = 9, style = "vertical-align: top; padding: 10px;", tags$img(src = img_src, alt = drug$Title, class = "img-fluid img-thumbnail", style = "max-width: 100%; max-height: 250px; object-fit: contain;")), tags$td(class = "key", style="font-weight: bold;", "Drug"), tags$td(class = "value", drug$Title) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "CID"), tags$td(class = "value", ifelse(is.na(drug$CID), "N/A", drug$CID)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "Formula"), tags$td(class = "value", ifelse(is.na(drug$MolecularFormula), "N/A", drug$MolecularFormula)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "Weight"), tags$td(class = "value", ifelse(is.na(drug$MolecularWeight), "N/A", drug$MolecularWeight)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "Description"), tags$td(class = "value", style="max-height: 100px; overflow-y: auto; display: block;", ifelse(is.na(drug$Description), "N/A", drug$Description)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "IUPAC Name"), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug$IUPACName), "N/A", drug$IUPACName)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "InChI"), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug$InChI), "N/A", drug$InChI)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "InChIKey"), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug$InChIKey), "N/A", drug$InChIKey)) ),
              tags$tr( tags$td(class = "key", style="font-weight: bold;", "SMILES"), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug$CanonicalSMILES), "N/A", drug$CanonicalSMILES)) )
            )
          )
        })
      }
      remove_modal_spinner()
    }

    observe({
      updatePickerInput(session, "selected_drug", 
                        choices = drug_choices, 
                        selected = "Rifampin")
    })

    observeEvent(input$selected_drug, { 
      if (!initial_run_done() && req(input$selected_drug) == "Rifampin") {
        perform_drug_analysis("Rifampin")
        initial_run_done(TRUE)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$drug_onto_submit, {
      perform_drug_analysis(input$selected_drug)
    }, ignoreNULL = TRUE)
    
    observeEvent(input$download_drug_csv, {
      req(full_drug_data_for_download())
      
      filename <- paste0(input$selected_drug, "_Genes.csv")
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_drug_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_drug_excel, {
      req(full_drug_data_for_download())
      
      filename <- paste0(input$selected_drug, "_Genes.xlsx")
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_drug_data_for_download(), temp_file)
        
        file_content <- readBin(temp_file, "raw", file.info(temp_file)$size)
        encoded_content <- base64enc::base64encode(file_content)
        
        session$sendCustomMessage(
          type = 'downloadExcel',
          message = list(
            filename = filename,
            content = encoded_content
          )
        )
        
        unlink(temp_file)
      } else {
        showNotification("Excel export requires writexl package. Downloading as CSV instead.", type = "warning")
        
        temp_file <- tempfile(fileext = ".csv")
        write.csv(full_drug_data_for_download(), temp_file, row.names = FALSE)
        
        session$sendCustomMessage(
          type = 'downloadReady',
          message = list(
            filename = gsub("\\.xlsx$", ".csv", filename),
            data = paste(readLines(temp_file), collapse = "\n")
          )
        )
        
        unlink(temp_file)
      }
    })
    
    output$drugInfo <- renderUI({
        div(style="text-align: center; padding: 50px; color: grey;",
            p("Initializing Drug Information panel..."))
    })
    
    output$drug_gene_table <- renderDT({
        datatable(data.frame(Message = "Initializing..."), options = list(dom = 't'))
    }, server = TRUE)

  })
} 