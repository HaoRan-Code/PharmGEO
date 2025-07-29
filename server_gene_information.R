geneInformationServer <- function(id, gene_choices, hot_genes, connect_to_db, process_in_chunks) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_gene_for_analysis <- reactiveVal(NULL)
    
    full_gene_data_for_download <- reactiveVal(NULL)

    observeEvent(input$help_drugs, {
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
              " Gene symbols with high consistency (Top 25%) and low variability (Bottom 25%)"
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

    observe({
        updatePickerInput(session, "selected_gene", 
                          choices = gene_choices,
                          selected = "BRCA1")
    })
    
    output$hot_gene_buttons <- renderUI({
        lapply(hot_genes, function(gene) {
            actionLink(
                inputId = ns(paste0("hot_gene_", gene)),
                label = gene,
                style = "background: linear-gradient(135deg, rgba(65, 88, 208, 0.1) 0%, rgba(200, 80, 192, 0.1) 100%); 
                          padding: 6px 12px; 
                          border-radius: 20px; 
                          display: inline-block; 
                          font-size: 0.85rem;
                          transition: all 0.3s;
                          color: #4158D0;
                          font-weight: 500;
                          border: 1px solid rgba(65, 88, 208, 0.2);",
                class = "hot-gene-link"
            )
        })
    })
    
    lapply(hot_genes, function(gene) {
        observeEvent(input[[paste0("hot_gene_", gene)]], {
            updatePickerInput(session, "selected_gene", selected = gene)
            selected_gene_for_analysis(gene)
        })
    })
    
    observeEvent(input$gene_onto_submit, {
        req(input$selected_gene)
        selected_gene_for_analysis(input$selected_gene)
    })
    
    observeEvent(input$selected_gene, { 
        if (!is.null(input$selected_gene) && input$selected_gene == "BRCA1") {
             selected_gene_for_analysis("BRCA1")
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE)
    
    observeEvent(selected_gene_for_analysis(), {
        current_gene <- selected_gene_for_analysis()
        req(current_gene)
        
        show_modal_spinner(spin = "flower", color = "#2c3e50", text = paste("Loading data for", current_gene, "..."))
        
        db <- connect_to_db("drug_gene")
        
        tryCatch({
            gene_db_con <- connect_to_db("gene_info")
            if (is.null(gene_db_con)) {
                stop("Gene info database connection failed.")
            }

            tables <- dbListTables(gene_db_con)
            if (length(tables) > 0) {
              table_name <- tables[1]
              query <- sprintf("SELECT * FROM %s WHERE name = ? OR gene_name = ?", table_name)
              gene_data <- dbGetQuery(gene_db_con, query, params = list(current_gene, current_gene))
              
              if (nrow(gene_data) > 0) {
                  gene_data <- gene_data[1, ]
                  output$geneInfo <- renderUI({
                    tags$table(
                      class = "layui-table mt-0",
                      tags$colgroup( tags$col(width = "30%"), tags$col(width = "70%") ),
                      tags$tbody(
                        tags$tr( tags$td(class = "key", strong("Gene Symbol")), tags$td(class = "value", gene_data$name) ),
                        tags$tr( tags$td(class = "key", strong("Gene Name")), tags$td(class = "value", ifelse(is.na(gene_data$gene_name), "N/A", gene_data$gene_name)) ),
                        tags$tr( tags$td(class = "key", strong("Location")), tags$td(class = "value", ifelse(is.na(gene_data$chromosome), "N/A", gene_data$chromosome)) ),
                        tags$tr( tags$td(class = "key", strong("Entrez ID")), tags$td(class = "value", ifelse(is.na(gene_data$entrez_id), "N/A", gene_data$entrez_id)) ),
                        tags$tr( tags$td(class = "key", strong("MIM ID")), tags$td(class = "value", ifelse(is.na(gene_data$mim_id), "N/A", gene_data$mim_id)) ),
                        tags$tr( tags$td(class = "key", strong("Aliases")), tags$td(class = "value", ifelse(is.na(gene_data$otheraliases), "N/A", gene_data$otheraliases)) ),
                        tags$tr( tags$td(class = "key", strong("Description")), tags$td(class = "value", ifelse(is.na(gene_data$description), "N/A", gene_data$description)) ),
                        tags$tr( tags$td(class = "key", strong("Summary")), tags$td(class = "value", style="max-height: 150px; overflow-y: auto; display: block;", ifelse(is.na(gene_data$summary), "N/A", gene_data$summary)) ),
                        tags$tr( tags$td(class = "key", strong("GeneCards")), tags$td(class = "value", tags$a(href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", current_gene), target = "_blank", "View on GeneCards")) )
                      )
                    )
                  })
              } else {
                  output$geneInfo <- renderUI({
                      div( style = "text-align: center; padding: 20px; color: grey;",
                           h4(paste("Detailed information for", current_gene, "not found in local database.")),
                           tags$p(tags$a(href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", current_gene), target = "_blank", paste("View", current_gene, "on GeneCards"))) )
                  })
              }
            } else {
                 stop("No tables found in gene info database.")
            }
        }, error = function(e) {
             output$geneInfo <- renderUI({
                 div( style = "text-align: center; padding: 20px; color: red;",
                      h4("Failed to retrieve gene information."),
                      tags$p(paste("Error:", e$message)),
                      tags$p(tags$a(href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", current_gene), target = "_blank", paste("Try viewing", current_gene, "on GeneCards"))) )
             })
        })

        query_drugs <- sprintf("
          SELECT standard_name, genes, GMCS, Level_3_Code, Drug_Name_3, pubmed_count 
          FROM merged_results_grouped 
          WHERE genes = '%s'", current_gene)
        
        gene_query_res <- dbGetQuery(db, query_drugs)
        
        if (nrow(gene_query_res) > 0) {
            processed_data <- reactiveVal(NULL)
            
            process_chunk <- function(chunk) {
              chunk$pubmed_count_numeric <- chunk$pubmed_count
              chunk <- chunk[order(-chunk$pubmed_count_numeric), ]
              chunk$standard_name_link <- paste0('<a href="https://pubchem.ncbi.nlm.nih.gov/#query=', URLencode(chunk$standard_name), '" target="_blank">', chunk$standard_name, '</a>')
              chunk$genes_link <- paste0('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=', chunk$genes, '" target="_blank">', chunk$genes, '</a>')
              search_terms <- paste0('"', chunk$standard_name, '" AND "', chunk$genes, '"')
              pubmed_urls  <- paste0('https://pubmed.ncbi.nlm.nih.gov/?term=', URLencode(search_terms, reserved = TRUE))
              chunk$pubmed_link <- paste0('<a href="', pubmed_urls, '" target="_blank">', chunk$pubmed_count, '</a>')
              create_atc_links <- function(codes) {
                if (is.na(codes) || codes == "") return("N/A")
                codes_list <- unlist(strsplit(codes, ","))
                codes_list <- trimws(codes_list)
                links <- paste0('<a href="https://www.whocc.no/atc_ddd_index/?code=', codes_list, '" target="_blank">', codes_list, '</a>')
                paste(links, collapse = ", ")
              }
              chunk$Level_3_Code_link <- sapply(chunk$Level_3_Code, create_atc_links)
              chunk$GMCS <- as.numeric(chunk$GMCS)
              chunk_display <- chunk[, c("standard_name_link", "genes_link", "GMCS", "Level_3_Code_link", "Drug_Name_3", "pubmed_link")]
              colnames(chunk_display) <- c("Standard Name", "Genes", "GMCS", "ATC Level 3", "Drug Name (ATC L3)", "PubMed Count")
              return(chunk_display)
            }
            
            all_processed_chunks <- list()
            update_progress <- function(i, total, result) {
              incProgress(1/total, detail = paste0("Processing drug batch ", i, " of ", total))
              all_processed_chunks[[i]] <<- result
            }
            
            withProgress(message = 'Processing associated drugs...', value = 0, {
                process_in_chunks(gene_query_res, chunk_size = 50, process_chunk, update_progress)
            })
            
            final_processed_data <- do.call(rbind, all_processed_chunks)
            processed_data(final_processed_data)
            
            download_data <- final_processed_data
            download_data$`Standard Name` <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$`Standard Name`)
            download_data$Genes <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$Genes)
            download_data$`ATC Level 3` <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$`ATC Level 3`)
            download_data$`PubMed Count` <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$`PubMed Count`)
            full_gene_data_for_download(download_data)
                
            output$gene_drug_table <- renderDT({
              req(processed_data())
              
              datatable(
                processed_data(),
                escape = FALSE,
                filter = "top",
                rownames = FALSE,
                options = list(
                  pageLength = 10,
                  lengthMenu = c(10, 20, 50, 100),
                  autoWidth = TRUE,
                  dom = 'Blfrtip',
                  buttons = list(list(
                    extend = 'collection',
                    buttons = list(
                      list(extend = 'csv', 
                           text = 'Download CSV (Full Data)',
                           action = DT::JS("function ( e, dt, node, config ) {
                             Shiny.setInputValue('geneInfo-download_gene_csv', true, {priority: 'event'});
                           }")),
                      list(extend = 'excel', 
                           text = 'Download Excel (Full Data)',
                           action = DT::JS("function ( e, dt, node, config ) {
                             Shiny.setInputValue('geneInfo-download_gene_excel', true, {priority: 'event'});
                           }"))
                    ),
                    text = 'Download Full Data'
                  )),
                  lengthChange = TRUE,
                  scrollX = TRUE,
                  scrollY = "450px",
                  scrollCollapse = TRUE,
                  columnDefs = list(
                    list(orderable = FALSE, targets = 5),
                    list(className = 'dt-center', targets = "_all")
                  )
                ),
                extensions = 'Buttons',
                selection = "none"
              ) %>% formatRound(columns = c("GMCS"), digits = 3)
            }, server = TRUE)
            
        } else {
            output$gene_drug_table <- renderDT({
                datatable(data.frame(Message = paste("No associated drugs found for gene:", current_gene)), options = list(dom = 't'))
            }, server = TRUE)
        }
        
        remove_modal_spinner()
        
    }, ignoreNULL = TRUE)
    
    observeEvent(input$download_gene_csv, {
      req(full_gene_data_for_download(), selected_gene_for_analysis())
      
      filename <- paste0(selected_gene_for_analysis(), "_Drugs.csv")
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_gene_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_gene_excel, {
      req(full_gene_data_for_download(), selected_gene_for_analysis())
      
      filename <- paste0(selected_gene_for_analysis(), "_Drugs.xlsx")
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_gene_data_for_download(), temp_file)
        
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
        write.csv(full_gene_data_for_download(), temp_file, row.names = FALSE)
        
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
    
    output$geneInfo <- renderUI({
        div(style="text-align: center; padding: 50px; color: grey;",
            p("Select a gene or click a hot gene, then click 'Analyze' to view details."))
    })
    output$gene_drug_table <- renderDT({
        datatable(data.frame(Message = "Select a gene and click 'Analyze' to view associated drugs."), options = list(dom = 't'))
    }, server = TRUE)

  })
} 