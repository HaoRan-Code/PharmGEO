rnaseqDatasetsServer <- function(id, dd_interaction_data, infor_matrix_data, connect_to_db) {
  moduleServer(id, function(input, output, session) {

    submission_done <- reactiveVal(FALSE)
    selected_drug_id <- reactiveVal(NULL)
    
    full_diff_data_for_download <- reactiveVal(NULL)
    full_metadata_for_download <- reactiveVal(NULL)
    full_kegg_data_for_download <- reactiveVal(NULL)
    full_go_data_for_download <- reactiveVal(NULL)
    full_gsea_data_for_download <- reactiveVal(NULL)

    output$dd_interaction_output <- renderDT({
      local_dd_interaction <- dd_interaction_data
      
      local_dd_interaction$gse_id <- paste0(
        '<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', 
        local_dd_interaction$gse_id,
        '" target="_blank">',
        local_dd_interaction$gse_id, 
        '</a>'
      )
      
      datatable(
        local_dd_interaction,
        escape     = FALSE,
        selection  = "single", 
        filter     = "top",
                    rownames   = FALSE,
        options    = list(
          pageLength  = 5,
          lengthMenu  = c(5, 10, 50, 100),
                        autoWidth   = FALSE,
          filtering   = TRUE,
          ordering    = TRUE,
          dom         = 'Blfrtip',
          lengthChange= TRUE,
          scrollX = TRUE,
                      scrollY = TRUE,
          scrollCollapse = FALSE,
          buttons     = list(
            list(
              extend  = 'collection',
              buttons = list(
                list(extend = 'csv', 
                     text = 'Download CSV (Full Data)',
                     action = DT::JS("function ( e, dt, node, config ) {
                       Shiny.setInputValue('rnaseq-download_dataset_csv', true, {priority: 'event'});
                     }")),
                list(extend = 'excel', 
                     text = 'Download Excel (Full Data)',
                     action = DT::JS("function ( e, dt, node, config ) {
                       Shiny.setInputValue('rnaseq-download_dataset_excel', true, {priority: 'event'});
                     }"))
              ),
              text    = 'Download Full Data'
            )
          ),
          columnDefs = list(
              list(className = 'dt-left', targets = 0),
              list(className = 'dt-center', targets = c(1, 2, 3, 4, 5))
          )
        ),
        extensions = 'Buttons'
      ) %>% formatStyle(columns = 1, fontWeight = 'bold')
    }, server = TRUE)
    
    observeEvent(input$drug_gene_submit, {
      show_modal_spinner(spin = "flower", color = "#2c3e50", text = "Analysing, please wait...")
      
      db         <- connect_to_db("diffsig")
      result_db  <- connect_to_db("analysis_results")
      
      selected_row <- input$dd_interaction_output_rows_selected
      if (length(selected_row) == 0) {
        shinyalert("Warning", "Please select a row before submitting.", type = "error")
        remove_modal_spinner()
        return()
      }
      
      drug_id <- dd_interaction_data[selected_row, "drug_id"]
      selected_drug_id(drug_id)
      
      query <- sprintf("
        SELECT genes, logFC, \"p.value\" 
        FROM combined_diffsig
        WHERE ID = '%s'
      ", selected_drug_id())
      
      diff_data <- dbGetQuery(db, query)
      
      if (nrow(diff_data) > 0) {
        diff_data$Regulation <- ifelse(diff_data$logFC > 0, "Upregulated", "Downregulated")
        diff_data$label      <- paste("Gene Symbol:", diff_data$genes)
        
        diff_download_data <- diff_data[, c("genes", "logFC", "p.value", "Regulation")]
        full_diff_data_for_download(diff_download_data)
        
        p <- ggplot(diff_data, aes(x = logFC, y = -log10(p.value), color = Regulation, text = label)) +
          geom_point(alpha = 0.8, size = 2) +
          scale_color_manual(values = c("Upregulated" = "#E32722", "Downregulated" = "#3082B9")) +
          theme_minimal() +
          labs(x = "Log2 Fold Change", y = "-Log10 P-value") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
          theme(legend.position = "right")
        
        plotly_plot <- ggplotly(p, tooltip = c("x", "y", "text"))
      } else {
        plotly_plot <- plotly_empty() %>% layout(title = "No differential expression data found.")
      }
      
      output$volcano_plot <- renderPlotly({
        plotly_plot
      })
      
      output$diff_data_table <- renderDT({
          if (nrow(diff_data) == 0) {
              return(datatable(data.frame(Message = "No differential expression data available."), options = list(dom = 't')))
          }
          diff_data_display <- diff_data
          diff_data_display$genes <- paste0(
              '<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=',
              diff_data_display$genes,
              '" target="_blank">',
              diff_data_display$genes,
              '</a>'
          )
          
          datatable(
              diff_data_display[, c("genes", "logFC", "p.value")],
              selection = "none",
              filter    = "top",
              escape    = FALSE,
              rownames  = FALSE,
              options   = list(
                  pageLength  = 5,
                  lengthMenu  = c(5, 10, 50, 100),
                  autoWidth   = FALSE,
                  dom         = 'Blfrtip',
                  buttons     = list(
                      list(
                          extend  = 'collection',
                          buttons = list(
                              list(extend = 'csv', 
                                   text = 'Download CSV (Full Data)',
                                   action = DT::JS("function ( e, dt, node, config ) {
                                     Shiny.setInputValue('rnaseq-download_diff_csv', true, {priority: 'event'});
                                   }")),
                              list(extend = 'excel', 
                                   text = 'Download Excel (Full Data)',
                                   action = DT::JS("function ( e, dt, node, config ) {
                                     Shiny.setInputValue('rnaseq-download_diff_excel', true, {priority: 'event'});
                                   }"))
                          ),
                          text    = 'Download Full Data'
                      )
                  ),
                  scrollX = TRUE,
                  scrollY = "300px",
                  scrollCollapse = TRUE,
                  columnDefs = list(
                      list(className = 'dt-left', targets = 0),
                      list(className = 'dt-center', targets = c(1, 2))
                  )
              ),
              extensions = 'Buttons'
          ) %>% formatRound(columns = c("logFC", "p.value"), digits = 2)
      }, server = TRUE)
      
      metadata_rows <- infor_matrix_data[infor_matrix_data$drug_id %in% selected_drug_id(), ]
      
      output$metadata_table <- renderDT({
        if (nrow(metadata_rows) == 0) {
          return(datatable(data.frame(Message = "No metadata available."), options = list(dom = 't')))
        }
        
        metadata_selected <- t(metadata_rows)
        colnames(metadata_selected) <- "information"
        rowname_save <- rownames(metadata_selected)
        
        metadata_selected_df <- data.frame(
          item        = rowname_save,
          information = as.character(metadata_selected[,1]),
          stringsAsFactors = FALSE
        )
        
        metadata_selected_df$information <- sapply(metadata_selected_df$information, function(info_item) {
           if (grepl("^GSE", info_item)) {
               paste0('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', info_item, '" target="_blank">', info_item, '</a>')
           } else if (grepl("^GSM", info_item)) {
               ids <- unlist(strsplit(info_item, "\\|"))
               paste0('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=', ids, '" target="_blank">', ids, '</a>', collapse = " | ")
           } else {
               info_item
           }
        })
        
        ideal_order <- c(
          "standard_name", "drug_name", "organism", "cell_type", "dose", "duration",
          "pert_name", "gse_id", "GPL", "ctrl_ids", "pert_ids", "type", "Exp_type", "drug_id"
        )
        metadata_selected_df$item <- factor(metadata_selected_df$item, levels = ideal_order)
        metadata_selected_df <- metadata_selected_df[order(metadata_selected_df$item), ]
        rownames(metadata_selected_df) <- NULL
        
        metadata_download <- metadata_selected_df
        metadata_download$information <- gsub('<a[^>]*>([^<]+)</a>', '\\1', metadata_download$information)
        full_metadata_for_download(metadata_download)
        
        datatable(
          metadata_selected_df,
          selection = "none",
          escape    = FALSE,
          rownames  = FALSE,
          options   = list(
            pageLength  = 16,
            autoWidth   = TRUE,
            dom         = 'Blfrtip',
            buttons     = list(
              list(
                extend  = 'collection',
                buttons = list(
                  list(extend = 'csv', 
                       text = 'Download CSV (Full Data)',
                       action = DT::JS("function ( e, dt, node, config ) {
                         Shiny.setInputValue('rnaseq-download_metadata_csv', true, {priority: 'event'});
                       }")),
                  list(extend = 'excel', 
                       text = 'Download Excel (Full Data)',
                       action = DT::JS("function ( e, dt, node, config ) {
                         Shiny.setInputValue('rnaseq-download_metadata_excel', true, {priority: 'event'});
                       }"))
                ),
                text    = 'Download Full Data'
              )
            ),
            columnDefs = list(
              list(className = 'dt-left', targets = 0),
              list(className = 'dt-center', targets = 1)
            )
          ),
          extensions = 'Buttons'
        ) %>% formatStyle(
          'item', 
          fontWeight = 'bold'
        )
      }, server = TRUE)
      
      render_enrichment <- function(type) {
        query <- sprintf("SELECT * FROM %s WHERE drugID = '%s'", type, selected_drug_id())
        data <- tryCatch({ dbGetQuery(result_db, query) }, error = function(e) { NULL })
        
        if (!is.null(data) && nrow(data) > 0) {
          if (type == "KEGG") {
            full_kegg_data_for_download(data)
          } else if (type == "GO") {
            full_go_data_for_download(data)
          } else if (type == "GSEA") {
            full_gsea_data_for_download(data)
          }
        }
        
        output[[paste0(tolower(type), "_plot")]] <- renderPlotly({
          if (is.null(data) || nrow(data) == 0) {
            plotly_empty() %>% layout(title = paste("No", type, "results found."))
          } else {
            plot_data <- data %>% 
              filter(pvalue < 0.05) %>% 
              head(15)  
            if (nrow(plot_data) == 0) return(plotly_empty() %>% layout(title = paste("No significant", type, "results (P < 0.05).")))
            
            plot_data$Description <- factor(plot_data$Description, levels = plot_data$Description[order(plot_data$Count)])
            
            if (type == "GSEA") {
              plot_data$Description <- factor(plot_data$Description, levels = plot_data$Description[order(plot_data$RichFactor)])
               p <- ggplot(plot_data, aes(x = RichFactor, y = Description, fill = pvalue, size = Count)) +
                   geom_point(shape = 21) +
                   scale_fill_viridis_c(option = "viridis") +
                   scale_size_continuous(range = c(3, 8)) +
                   labs(x = "Normalized Enrichment Score (RichFactor)", y = "Description", fill = "pvalue", size = "Count") +
                   theme_minimal() + 
                   theme(axis.text.y = element_text(size=10))
            } else {
               p_col <- "pvalue"
               p <- ggplot(plot_data, aes(x = Count, y = Description,  fill = !!sym(p_col), size = Count)) +
                  geom_point() +
                  scale_fill_viridis_c(option = "viridis") +
                  scale_size_continuous(range = c(3,8)) +
                  labs(color = ifelse(p_col == "p.adjust", "Adj. P-value", "P-value")) +
                  theme_minimal() + 
                  theme(axis.text.y = element_text(size=10))
            }
            ggplotly(p)
          }
        })
        
        output[[paste0(tolower(type), "_table")]] <- renderDT({
          if (is.null(data) || nrow(data) == 0) {
            datatable(data.frame(Message = paste("No", type, "results available.")), options = list(dom = 't'))
          } else {
            datatable(
              data,
              selection = "none",
              filter    = "top",
              rownames  = FALSE,
              class = 'no-fixed-layout display',
              options   = list(
                pageLength  = 10,
                lengthMenu  = c(10, 20, 50, 100),
                autoWidth   = TRUE,
                dom         = 'Blfrtip',
                buttons     = list(
                  list(
                    extend  = 'collection',
                    buttons = list(
                      list(extend = 'csv', 
                           text = 'Download CSV (Full Data)',
                           action = DT::JS(paste0("function ( e, dt, node, config ) {
                             Shiny.setInputValue('rnaseq-download_", tolower(type), "_csv', true, {priority: 'event'});
                           }"))),
                      list(extend = 'excel', 
                           text = 'Download Excel (Full Data)',
                           action = DT::JS(paste0("function ( e, dt, node, config ) {
                             Shiny.setInputValue('rnaseq-download_", tolower(type), "_excel', true, {priority: 'event'});
                           }")))
                    ),
                    text    = 'Download Full Data'
                  )
                ),
                scrollX = TRUE,
                scrollY = TRUE,
                scrollCollapse = FALSE,
                columnDefs = list(
                  list(className = 'dt-left', targets = 0),
                  list(className = 'dt-center', targets = "_all")
                )
              ),
              extensions = 'Buttons'
            )
          }
        }, server = TRUE)
      }
      
      render_enrichment("KEGG")
      render_enrichment("GO")
      render_enrichment("GSEA")
      
      delay(100, { runjs('window.scrollBy({ top: window.innerHeight * 0.8, behavior: "smooth" });') })
      submission_done(TRUE)
      remove_modal_spinner()
    })
    
    output$additional_content <- renderUI({
      if (submission_done()) {
        tabsetPanel(
          id   = session$ns("nav_menu"),
          type = "tabs",
          
          tabPanel("Metadata",
                   fluidRow(
                     column(6,offset = 3,
                            box(
                              title       = strong("Metadata"),
                              status      = "primary",
                              solidHeader = TRUE,
                              width       = 12,
                              style       = 'overflow-y: auto; margin-bottom: 20px;',
                              div(
                                style = "padding: 10px 20px; margin-bottom: 15px; background: #f8faff; border-radius: 8px; border-left: 4px solid #4158D0;",
                                div(
                                  style = "display: flex; align-items: center; justify-content: space-between;",
                                  span(
                                    style = "color: #4158D0; font-weight: 500;",
                                    icon("info-circle", lib = "font-awesome", style = "margin-right: 8px;"),
                                    "Need help understanding the metadata fields?"
                                  ),
                                  actionButton(
                                    inputId = session$ns("help_metadata"),
                                    label = "View Field Definitions",
                                    icon = icon("circle-info", lib = "font-awesome"),
                                    class = "btn btn-outline-primary btn-sm",
                                    style = "font-size: 12px; padding: 4px 12px;"
                                  )
                                )
                              ),
                              DTOutput(session$ns("metadata_table"))
                            )
                     )
                   )
          ),
          
          tabPanel("Differential Expression Analysis",
                   fluidRow(
                     column(6,
                            box(
                              title       = strong("Volcano Plot"),
                              status      = "primary",
                              solidHeader = TRUE,
                              width       = 12,
                              style       = 'margin-bottom: 20px;',
                              plotlyOutput(session$ns("volcano_plot"))
                            )
                     ),
                     column(6,
                            box(
                              title       = strong("Differential Expression Genes"),
                              status      = "primary",
                              solidHeader = TRUE,
                              width       = 12,
                              style       = 'margin-bottom: 20px;',
                              DTOutput(session$ns("diff_data_table"))
                            )
                     )
                   )
          ),
          
          tabPanel("Enrichment Analysis",
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("KEGG",
                              fluidRow(
                                column(6,
                                       box(
                                         title       = strong("KEGG Plot"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         plotlyOutput(session$ns("kegg_plot"))
                                       )
                                ),
                                column(6,
                                       box(
                                         title       = strong("KEGG Table"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         DTOutput(session$ns("kegg_table"))
                                       )
                                )
                              )
                     ),
                     tabPanel("GO",
                              fluidRow(
                                column(6,
                                       box(
                                         title       = strong("GO Plot"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         plotlyOutput(session$ns("go_plot"))
                                       )
                                ),
                                column(6,
                                       box(
                                         title       = strong("GO Table"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         DTOutput(session$ns("go_table"))
                                       )
                                )
                              )
                     ),
                     tabPanel("GSEA",
                              fluidRow(
                                column(6,
                                       box(
                                         title       = strong("GSEA Plot"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         plotlyOutput(session$ns("gsea_plot"))
                                       )
                                ),
                                column(6,
                                       box(
                                         title       = strong("GSEA Table"),
                                         status      = "primary",
                                         solidHeader = TRUE,
                                         width       = 12,
                                         style       = 'margin-bottom: 20px;',
                                         DTOutput(session$ns("gsea_table"))
                                       )
                                )
                              )
                     )
                   )
          )
        )
      } else {
          div(style="text-align:center; padding: 20px; color: grey;", 
              p("Select a dataset and click 'Analyze' to view results."))
      }
    })
    
    observeEvent(input$help_metadata, {
      showModal(modalDialog(
        title = "Metadata Field Definitions",
        div(
          style = "max-height: 60vh; overflow-y: auto; padding: 20px;",
          h4("Field Explanations", style = "color: #4158D0; margin-bottom: 20px;"),
          
          div(style = "margin-bottom: 15px;",
              strong("standard_name:", style = "color: #4158D0;"),
              " Standardized drug name using PubChem database nomenclature"
          ),
          div(style = "margin-bottom: 15px;",
              strong("drug_name:", style = "color: #4158D0;"),
              " Original drug name as recorded in the GEO database"
          ),
          div(style = "margin-bottom: 15px;",
              strong("organism:", style = "color: #4158D0;"),
              " Species of the sequenced samples"
          ),
          div(style = "margin-bottom: 15px;",
              strong("cell_type:", style = "color: #4158D0;"),
              " Cell line or tissue type used in the sequencing experiment"
          ),
          div(style = "margin-bottom: 15px;",
              strong("dose:", style = "color: #4158D0;"),
              " Drug dosage concentration administered"
          ),
          div(style = "margin-bottom: 15px;",
              strong("duration:", style = "color: #4158D0;"),
              " Duration of drug treatment"
          ),
          div(style = "margin-bottom: 15px;",
              strong("gse_id:", style = "color: #4158D0;"),
              " Unique identifier for each dataset in the GEO database (clickable link to GEO page)"
          ),
          div(style = "margin-bottom: 15px;",
              strong("GPL:", style = "color: #4158D0;"),
              " Sequencing platform identifier providing technical parameter information"
          ),
          div(style = "margin-bottom: 15px;",
              strong("ctrl_ids:", style = "color: #4158D0;"),
              " Sample identifiers used as control group in differential expression analysis"
          ),
          div(style = "margin-bottom: 15px;",
              strong("pert_ids:", style = "color: #4158D0;"),
              " Sample identifiers used as treatment group in differential expression analysis"
          ),
          div(style = "margin-bottom: 15px;",
              strong("type:", style = "color: #4158D0;"),
              " Perturbation type - PharmGEO database exclusively contains drug-type perturbations"
          ),
          div(style = "margin-bottom: 15px;",
              strong("Exp_type:", style = "color: #4158D0;"),
              " Experimental platform type: includes 4,438 microarray datasets (Expression profiling by array) and 3,342 high-throughput sequencing datasets (Expression profiling by high throughput sequencing)"
          )
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$download_dataset_csv, {
      req(dd_interaction_data)
      
      filename <- "Datasets.csv"
      
      download_data <- dd_interaction_data
      download_data$gse_id <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$gse_id)
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(download_data, temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_dataset_excel, {
      req(dd_interaction_data)
      
      filename <- "Datasets.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(download_data, temp_file)
        
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
        write.csv(download_data, temp_file, row.names = FALSE)
        
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
    
    observeEvent(input$download_diff_csv, {
      req(full_diff_data_for_download())
      
      filename <- "Differential_Expression.csv"
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_diff_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_diff_excel, {
      req(full_diff_data_for_download())
      
      filename <- "Differential_Expression.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_diff_data_for_download(), temp_file)
        
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
        write.csv(full_diff_data_for_download(), temp_file, row.names = FALSE)
        
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
    
    observeEvent(input$download_metadata_csv, {
      req(full_metadata_for_download())
      
      filename <- "Metadata.csv"
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_metadata_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_metadata_excel, {
      req(full_metadata_for_download())
      
      filename <- "Metadata.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_metadata_for_download(), temp_file)
        
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
        write.csv(full_metadata_for_download(), temp_file, row.names = FALSE)
        
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
    
    observeEvent(input$download_kegg_csv, {
      req(full_kegg_data_for_download())
      
      filename <- "KEGG_Results.csv"
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_kegg_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_kegg_excel, {
      req(full_kegg_data_for_download())
      
      filename <- "KEGG_Results.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_kegg_data_for_download(), temp_file)
        
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
        write.csv(full_kegg_data_for_download(), temp_file, row.names = FALSE)
        
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
    
    observeEvent(input$download_go_csv, {
      req(full_go_data_for_download())
      
      filename <- "GO_Results.csv"
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_go_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_go_excel, {
      req(full_go_data_for_download())
      
      filename <- "GO_Results.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_go_data_for_download(), temp_file)
        
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
        write.csv(full_go_data_for_download(), temp_file, row.names = FALSE)
        
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
    
    observeEvent(input$download_gsea_csv, {
      req(full_gsea_data_for_download())
      
      filename <- "GSEA_Results.csv"
      
      temp_file <- tempfile(fileext = ".csv")
      write.csv(full_gsea_data_for_download(), temp_file, row.names = FALSE)
      
      session$sendCustomMessage(
        type = 'downloadReady',
        message = list(
          filename = filename,
          data = paste(readLines(temp_file), collapse = "\n")
        )
      )
      
      unlink(temp_file)
    })
    
    observeEvent(input$download_gsea_excel, {
      req(full_gsea_data_for_download())
      
      filename <- "GSEA_Results.xlsx"
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl", quiet = TRUE)
      }
      
      if (requireNamespace("writexl", quietly = TRUE)) {
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(full_gsea_data_for_download(), temp_file)
        
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
        write.csv(full_gsea_data_for_download(), temp_file, row.names = FALSE)
        
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

  })
} 