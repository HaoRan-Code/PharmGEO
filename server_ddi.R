ddiServer <- function(id, connect_to_db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    drug_choices_cache <- reactiveVal(NULL)
    drug_b_options <- reactiveVal(NULL)
    initial_echart_cleared <- reactiveVal(FALSE)
    full_intersect_data <- reactiveVal(NULL)
    current_drug_a <- reactiveVal(NULL)
    current_drug_b <- reactiveVal(NULL)
    initial_analysis_done <- reactiveVal(FALSE)
    full_table_data_for_download <- reactiveVal(NULL)

    observe({
      sql_union <- "
        SELECT DISTINCT Drug_name_A as drug FROM filtered_ddi_data
        UNION
        SELECT DISTINCT Drug_name_B as drug FROM filtered_ddi_data
        ORDER BY drug;
      "
      db <- connect_to_db("drug_interactions")
      all_drugs <- dbGetQuery(db, sql_union)
      
      drug_choices_cache(all_drugs$drug)
      
      updatePickerInput(session, "selected_drug_A",
                        choices = drug_choices_cache(),
                        selected = "Aspirin")
    }, priority = 10)

    output$drugA_title <- renderUI({
      h3(paste(ifelse(!is.null(input$selected_drug_A), input$selected_drug_A, "Drug A"), "Information"), 
           class = "gradient-header", 
           style = "border-bottom: 1px solid var(--border-light); padding-bottom: 12px;")
    })
    
    output$drugB_title <- renderUI({
      h3(paste(ifelse(!is.null(input$selected_drug_B), input$selected_drug_B, "Drug B"), "Information"), 
           class = "gradient-header", 
           style = "border-bottom: 1px solid var(--border-light); padding-bottom: 12px;")
    })
    
    output$gene_matrix_title <- renderUI({
      if (!is.null(input$selected_drug_A) && !is.null(input$selected_drug_B)) {
        strong(paste("Intersecting Genes between", input$selected_drug_A, "and", input$selected_drug_B))
      } else {
        strong("Intersecting Genes")
      }
    })
    
    output$ddi_details_title <- renderUI({
      if (!is.null(input$selected_drug_A) && !is.null(input$selected_drug_B)) {
        strong(paste(input$selected_drug_A, "vs", input$selected_drug_B, "- Drug-Drug Interaction Details"))
      } else {
        strong("Drug-Drug Interaction Details")
      }
    })
    
    output$network_title <- renderUI({
      if (!is.null(input$selected_drug_A) && !is.null(input$selected_drug_B)) {
        strong(tagList(icon("project-diagram"), paste(" ", input$selected_drug_A, "-", input$selected_drug_B, "Interaction Network")))
      } else {
        strong(tagList(icon("project-diagram"), " Interaction Network"))
      }
    })

    calculated_drug_b_partners <- reactive({
        req(input$selected_drug_A)
        selected_A <- input$selected_drug_A
        sql_drugB <- "
          SELECT DISTINCT 
            CASE WHEN Drug_name_A = ? THEN Drug_name_B
                 WHEN Drug_name_B = ? THEN Drug_name_A
            END AS partner
          FROM filtered_ddi_data
          WHERE (? IN (Drug_name_A, Drug_name_B))
            AND (partner IS NOT NULL) AND (partner != ?)
          ORDER BY partner;
        "
        
        db <- connect_to_db("drug_interactions")
        associated_drug_B <- dbGetQuery(db, sql_drugB, 
                                      params = list(selected_A, selected_A, selected_A, selected_A))
        
        unique(associated_drug_B$partner)
    })

    output$drugB_picker <- renderUI({
      req(calculated_drug_b_partners())
      valid_partners <- calculated_drug_b_partners()
      
      initial_selection_b <- NULL
      preferred_default_b <- "Ibuprofen"
      
      if (input$selected_drug_A == "Aspirin" && !initial_analysis_done()) { 
           if (preferred_default_b %in% valid_partners) {
               initial_selection_b <- preferred_default_b
           } else if (length(valid_partners) > 0) {
               initial_selection_b <- valid_partners[1]
           }
      } else {
          current_b <- isolate(input$selected_drug_B) 
          if (!is.null(current_b) && (current_b %in% valid_partners)) {
              initial_selection_b <- current_b
          } else if (length(valid_partners) > 0) {
              initial_selection_b <- valid_partners[1]
          }
      }

      pickerInput(
        inputId = ns("selected_drug_B"),
        label = h5(tags$b("Select Drug B:")),
        choices = valid_partners,
        selected = initial_selection_b,
        options = list(`live-search` = TRUE, placeholder = 'Select Drug B', 
                      container = "body", dropupAuto = FALSE, 
                      virtualScroll = TRUE, maxOptions = 50, 
                      liveSearchNormalize = TRUE)
      )
    })

    observeEvent(input$selected_drug_A, {
        req(initial_analysis_done())
        
        valid_partners <- calculated_drug_b_partners()
        drug_b_options(valid_partners)
        
        current_b_selection <- isolate(input$selected_drug_B)
        
        target_selection <- NULL
        if (is.null(current_b_selection) || !(current_b_selection %in% valid_partners)) {
            if (length(valid_partners) > 0) {
                target_selection <- valid_partners[1]
            }
        } else {
            target_selection <- current_b_selection 
        }
        
        updatePickerInput(session, "selected_drug_B", 
                          selected = target_selection)
                          
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    get_drug_info <- function(drug_name) {
        req(drug_name)
        sql_info <- "SELECT * FROM drug_info WHERE Title = ?;" 
        db_info <- connect_to_db("drug_interactions") 
        drug <- dbGetQuery(db_info, sql_info, params = list(drug_name))
        if (nrow(drug) == 0) NULL else drug[1, ]
    }
    
    render_drug_info_box <- function(drug_data) {
      if (is.null(drug_data)) return(h4("Drug information not found."))
      
      local_img_path <- file.path("www", "CID_images", paste0(drug_data$CID, ".png"))
      img_src <- if (!file.exists(local_img_path)) "CID_images/default.png" else file.path("CID_images", paste0(drug_data$CID, ".png"))
      
      tags$table(
          class = "layui-table mt-0", style = "border:1px solid #ccc; border-collapse:collapse; margin-bottom: 0;",
          tags$colgroup( tags$col(width = "26%"), tags$col(width = "20%"), tags$col(width = "") ),
          tags$tbody(
              tags$tr( 
                tags$td(rowspan = 9, style="vertical-align: middle; text-align: center;", 
                        tags$img(src = img_src, alt = drug_data$Title, class = "img-fluid img-thumbnail", 
                                style = "max-width: 100%; max-height: 250px; object-fit: contain;")), 
                tags$td(class = "key", strong("Drug")), 
                tags$td(class = "value", drug_data$Title) ),
              tags$tr( tags$td(class = "key", strong("CID")), tags$td(class = "value", ifelse(is.na(drug_data$CID), "N/A", drug_data$CID)) ),
              tags$tr( tags$td(class = "key", strong("Formula")), tags$td(class = "value", ifelse(is.na(drug_data$MolecularFormula), "N/A", drug_data$MolecularFormula)) ),
              tags$tr( tags$td(class = "key", strong("Weight")), tags$td(class = "value", ifelse(is.na(drug_data$MolecularWeight), "N/A", drug_data$MolecularWeight)) ),
              tags$tr( tags$td(class = "key", strong("Description")), tags$td(class = "value", style="max-height: 100px; overflow-y: auto; display: block;", ifelse(is.na(drug_data$Description), "N/A", drug_data$Description)) ),
              tags$tr( tags$td(class = "key", strong("IUPAC Name")), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug_data$IUPACName), "N/A", drug_data$IUPACName)) ),
              tags$tr( tags$td(class = "key", strong("InChI")), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug_data$InChI), "N/A", drug_data$InChI)) ),
              tags$tr( tags$td(class = "key", strong("InChIKey")), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug_data$InChIKey), "N/A", drug_data$InChIKey)) ),
              tags$tr( tags$td(class = "key", strong("SMILES")), tags$td(class = "value", style="word-break: break-all;", ifelse(is.na(drug_data$CanonicalSMILES), "N/A", drug_data$CanonicalSMILES)) )
          )
      )
    }

    generate_network_graph_data <- function(intersect_data, top_n_val, drug_a_name, drug_b_name) {
        req(intersect_data, top_n_val, drug_a_name, drug_b_name)
        
        if(nrow(intersect_data) == 0) {
            return(list(nodes = list(), links = list(), categories = list()))
        }

        if (!"genes_original" %in% colnames(intersect_data)) {
            if ("genes" %in% colnames(intersect_data)) {
                intersect_data$genes_original <- intersect_data$genes
            } else {
                return(list(nodes = list(), links = list(), categories = list()))
            }
        }
        
        if (!"Category" %in% colnames(intersect_data)) {
            intersect_data$Category <- "Other"
        }
        
        if ("Category" %in% colnames(intersect_data) && !all(is.na(intersect_data$Category))) {
            valid_data <- intersect_data[!is.na(intersect_data$Category), ]
            
            if (nrow(valid_data) == 0) {
                return(list(nodes = list(), links = list(), categories = list()))
            }
            
            top_intersect_genes <- valid_data %>% 
                dplyr::group_by(Category) %>% 
                dplyr::arrange(p_value_A, .by_group = TRUE) %>% 
                dplyr::slice_head(n = as.integer(top_n_val)) %>% 
                dplyr::ungroup()
        } else {
            top_intersect_genes <- intersect_data %>% 
                dplyr::arrange(p_value_A) %>% 
                dplyr::slice_head(n = as.integer(top_n_val))
            
            if (!"Category" %in% colnames(top_intersect_genes)) {
                top_intersect_genes$Category <- "Other"
            }
        }
        
        if(nrow(top_intersect_genes) == 0) {
            return(list(nodes = list(), links = list(), categories = list()))
        }

        drug_a_category <- paste0("Drug_", gsub("[^A-Za-z0-9]", "_", drug_a_name))
        drug_b_category <- paste0("Drug_", gsub("[^A-Za-z0-9]", "_", drug_b_name))
        
        category_col <- if ("Category_Display" %in% colnames(top_intersect_genes)) {
            top_intersect_genes$Category_Display
        } else {
            top_intersect_genes$Category
        }
        
        nodes <- data.frame(
            id       = c("DrugA", top_intersect_genes$genes_original, "DrugB"),
            name     = c(drug_a_name, top_intersect_genes$genes_original, drug_b_name),
            category = c(drug_a_category, top_intersect_genes$Category, drug_b_category),
            display_category = c(drug_a_name, category_col, drug_b_name),
            stringsAsFactors = FALSE
        )
        
        links <- rbind(
            data.frame(source = "DrugA", target = top_intersect_genes$genes_original, stringsAsFactors = FALSE),
            data.frame(source = top_intersect_genes$genes_original, target = "DrugB", stringsAsFactors = FALSE)
        )
        
        all_cats <- unique(c(drug_a_category, drug_b_category, top_intersect_genes$Category))
        
        categories <- data.frame(
            name = all_cats,
            display_name = sapply(all_cats, function(cat) {
                if (cat == drug_a_category) return(drug_a_name)
                if (cat == drug_b_category) return(drug_b_name)
                if (cat %in% top_intersect_genes$Category) {
                    if ("Category_Display" %in% colnames(top_intersect_genes)) {
                        matching_display <- top_intersect_genes$Category_Display[top_intersect_genes$Category == cat]
                        if (length(matching_display) > 0) return(matching_display[1])
                    }
                }
                return(cat)
            }),
            stringsAsFactors = FALSE
        )
        
        nodes_list <- lapply(1:nrow(nodes), function(i) {
            list(
                id = as.character(nodes[i, "id"]),
                name = as.character(nodes[i, "name"]),
                category = as.character(nodes[i, "category"]),
                display_category = as.character(nodes[i, "display_category"])
            )
        })
        
        links_list <- lapply(1:nrow(links), function(i) {
            list(
                source = as.character(links[i, "source"]),
                target = as.character(links[i, "target"])
            )
        })
        
        categories_list <- lapply(1:nrow(categories), function(i) {
            list(
                name = as.character(categories[i, "name"]),
                display_name = as.character(categories[i, "display_name"])
            )
        })
        
        return(list(
            nodes = nodes_list,
            links = links_list,
            categories = categories_list,
            drug_a_category = drug_a_category,
            drug_b_category = drug_b_category
        ))
    }

    run_ddi_analysis <- function(selected_A, selected_B) {
        if (selected_A == selected_B) {
            shinyalert::shinyalert("Warning", "Please select two different drugs.", type = "warning")
            return(FALSE)
        }
        
        withProgress(message = "Analyzing Drug-Drug Interaction...", value = 0, {
            
            current_drug_a(selected_A)
            current_drug_b(selected_B)
            
            incProgress(0.1, detail = "Loading drug info...")
            
            drugA_data <- get_drug_info(selected_A)
            drugB_data <- get_drug_info(selected_B)
            
            output$drugA_info <- renderUI({ render_drug_info_box(drugA_data) })
            output$drugB_info <- renderUI({ render_drug_info_box(drugB_data) })
            
            incProgress(0.3, detail = "Querying intersecting genes...")
            
            sql_intersect <- "
              SELECT pA.genes AS genes, pA.logFC AS logFC_A, pA.p_value AS p_value_A, 
                     pA.direction AS direction_A, pB.logFC AS logFC_B, pB.p_value AS p_value_B, 
                     pB.direction AS direction_B 
              FROM processed_genes pA 
              JOIN processed_genes pB ON pA.genes = pB.genes 
              WHERE pA.drug_name = ? AND pB.drug_name = ?;
            "
            db <- connect_to_db("drug_interactions")
            intersect_genes <- dbGetQuery(db, sql_intersect, params = list(selected_A, selected_B))
            
            if (nrow(intersect_genes) > 0 && "genes" %in% colnames(intersect_genes)) {
                intersect_genes$genes_original <- intersect_genes$genes 
                
                drug_A_clean <- gsub("[^A-Za-z0-9]", "_", selected_A)
                drug_B_clean <- gsub("[^A-Za-z0-9]", "_", selected_B)
                
                intersect_genes$Category <- dplyr::case_when( 
                    intersect_genes$direction_A == "up" & intersect_genes$direction_B == "down" ~ paste0(drug_A_clean, "_up_", drug_B_clean, "_down"), 
                    intersect_genes$direction_A == "down" & intersect_genes$direction_B == "up" ~ paste0(drug_A_clean, "_down_", drug_B_clean, "_up"), 
                    intersect_genes$direction_A == "up" & intersect_genes$direction_B == "up" ~ paste0(drug_A_clean, "_up_", drug_B_clean, "_up"), 
                    intersect_genes$direction_A == "down" & intersect_genes$direction_B == "down" ~ paste0(drug_A_clean, "_down_", drug_B_clean, "_down"), 
                    TRUE ~ "Other" 
                )
                
                intersect_genes$Category_Display <- dplyr::case_when( 
                    intersect_genes$direction_A == "up" & intersect_genes$direction_B == "down" ~ paste0(selected_A, "↑ ", selected_B, "↓"), 
                    intersect_genes$direction_A == "down" & intersect_genes$direction_B == "up" ~ paste0(selected_A, "↓ ", selected_B, "↑"), 
                    intersect_genes$direction_A == "up" & intersect_genes$direction_B == "up" ~ paste0(selected_A, "↑ ", selected_B, "↑"), 
                    intersect_genes$direction_A == "down" & intersect_genes$direction_B == "down" ~ paste0(selected_A, "↓ ", selected_B, "↓"), 
                    TRUE ~ "Other" 
                )
                
                intersect_genes$genes_link <- paste0('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=', intersect_genes$genes_original, '" target="_blank">', intersect_genes$genes_original, '</a>')
            } else {
                 if(nrow(intersect_genes) > 0) {
                     if(!"Category" %in% colnames(intersect_genes)) intersect_genes$Category <- "Other"
                     if(!"Category_Display" %in% colnames(intersect_genes)) intersect_genes$Category_Display <- "Other"
                     if(!"genes_link" %in% colnames(intersect_genes)) intersect_genes$genes_link <- intersect_genes$genes 
                     if(!"genes_original" %in% colnames(intersect_genes)) intersect_genes$genes_original <- intersect_genes$genes
                 }
            }

            full_intersect_data(intersect_genes)
            
            incProgress(0.5, detail = "Preparing results table...")
            
            if (nrow(full_intersect_data()) == 0) { 
                output$gene_matrix <- renderDT({ datatable(data.frame(Message="No intersecting genes found."), options = list(dom = 't')) })
            } else {
                final_table_data <- full_intersect_data() %>% 
                                     dplyr::select(genes_link, logFC_A, p_value_A, direction_A, logFC_B, p_value_B, direction_B, Category_Display) 
                
                if ("Category_Display" %in% colnames(final_table_data)) {
                   colnames(final_table_data) <- c(
                       "Gene",
                       paste0("logFC_", selected_A),
                       paste0("p_value_", selected_A),
                       paste0("direction_", selected_A),
                       paste0("logFC_", selected_B),
                       paste0("p_value_", selected_B),
                       paste0("direction_", selected_B),
                       "Interaction_Pattern"
                   )
                   
                   logFC_A_col <- paste0("logFC_", selected_A)
                   p_value_A_col <- paste0("p_value_", selected_A)
                   logFC_B_col <- paste0("logFC_", selected_B)
                   p_value_B_col <- paste0("p_value_", selected_B)
                   
                   full_table_data_for_download(final_table_data)
                   
                   output$gene_matrix <- renderDT({ 
                       datatable( final_table_data, escape = FALSE, filter = "top", rownames = TRUE, 
                                  options = list( pageLength = 10, lengthMenu = c(10, 20, 50, 100), 
                                                autoWidth = TRUE, 
                                                dom = 'Blrtip', 
                                                buttons = list(list(extend = 'collection', 
                                                                  buttons = list(
                                                                    list(extend = 'csv', 
                                                                         text = 'Download CSV (Full Data)',
                                                                         action = DT::JS("function ( e, dt, node, config ) {
                                                                           Shiny.setInputValue('ddi-download_gene_csv', true, {priority: 'event'});
                                                                         }")),
                                                                    list(extend = 'excel', 
                                                                         text = 'Download Excel (Full Data)',
                                                                         action = DT::JS("function ( e, dt, node, config ) {
                                                                           Shiny.setInputValue('ddi-download_gene_excel', true, {priority: 'event'});
                                                                         }"))
                                                                  ), 
                                                                  text = 'Download Full Data')), 
                                                lengthChange = TRUE, scrollX = TRUE, scrollY = "300px", scrollCollapse = TRUE,
                                                columnDefs = list(
                                                  list(className = 'dt-center', targets = "_all")
                                                )), 
                                  extensions = 'Buttons', selection = "none" 
                       ) %>% formatRound(columns = c(logFC_A_col, p_value_A_col, logFC_B_col, p_value_B_col), digits = 2) 
                   }, server = TRUE)
                } else {
                    output$gene_matrix <- renderDT({ datatable(data.frame(Message="Error preparing table data."), options = list(dom = 't')) })
                }
            }
            
            incProgress(0.7, detail = "Generating initial network graph...")
            
            top_n_val <- isolate(input$top_n_genes)
            if (is.null(top_n_val) || top_n_val < 1) {
                top_n_val <- 10
            }
            
            if (nrow(full_intersect_data()) > 0) {
                initial_network_data <- generate_network_graph_data(full_intersect_data(), top_n_val, selected_A, selected_B)
                session$sendCustomMessage(type = 'echarts_data', initial_network_data)
            } else {
                session$sendCustomMessage(type = 'echarts_data', list(nodes = list(), links = list(), categories = list()))
            }

            incProgress(0.8, detail = "Querying DDI details...")
            
            ddi_sql <- "
              SELECT *
              FROM filtered_ddi_data
              WHERE (Drug_name_A = ? AND Drug_name_B = ?)
                 OR (Drug_name_A = ? AND Drug_name_B = ?);
            "
            ddi_data_all <- dbGetQuery(db, ddi_sql,
                                       params = list(selected_A, selected_B,
                                                     selected_B, selected_A))
            
            colorize_level <- function(level_value) {
                if (is.na(level_value)) level_value <- "none" 
                color <- dplyr::case_when(
                    level_value == "Major"    ~ "red",
                    level_value == "Minor"    ~ "orange",
                    level_value == "Moderate" ~ "blue",
                    level_value == "none"     ~ "grey",
                    TRUE ~ "black"
                )
                sprintf("<span style='color:%s;font-weight:bold;'>%s</span>", color, level_value)
            }
            
            render_ddi_table <- function(df_source) {
              if (nrow(df_source) == 0) {
                return(HTML("<p style='color:grey;font-style:italic;'>No interaction data available from this source for the selected drug pair.</p>"))
              }
              df_show <- df_source %>% 
                dplyr::select(Level, Interaction, Management, References) %>% 
                dplyr::mutate(Level = sapply(Level, colorize_level))
              
              {
                dt_widget <- DT::datatable(
                  df_show,
                  escape = FALSE,
                  rownames = FALSE,
                  selection = "none",
                  options = list(
                    pageLength = 5,
                    dom = 't',
                    autoWidth = TRUE,
                    scrollX = TRUE,
                    columnDefs = list(
                      list(className = 'dt-center', targets = 0),
                      list(className = 'dt-left', targets = c(1, 2, 3))
                    )
                  ),
                  class = 'display ddi-table'
                )
                shiny::tags$div(
                  style = 'width:100%; max-height:520px; overflow:auto;',
                  dt_widget
                )
              }
            }
            
            output$ddi_info_ddinter <- renderUI({ render_ddi_table(ddi_data_all %>% dplyr::filter(data_sources == "DDInter")) })
            output$ddi_info_mecddi <- renderUI({ render_ddi_table(ddi_data_all %>% dplyr::filter(data_sources == "MecDDI")) })
            output$ddi_info_rxnav <- renderUI({ render_ddi_table(ddi_data_all %>% dplyr::filter(data_sources == "RxNav")) })
            
            incProgress(1, detail = "Done")
            Sys.sleep(0.3)
        })
        
        return(TRUE)
    }
    
    observe({
      req(input$selected_drug_A, input$selected_drug_B, !initial_analysis_done())
      
      analysis_success <- run_ddi_analysis(isolate(input$selected_drug_A), isolate(input$selected_drug_B))
      
      if(isTRUE(analysis_success)){
          initial_analysis_done(TRUE)
      }
    }, priority = 0)

    observeEvent(input$go_button, {
        req(input$selected_drug_A, input$selected_drug_B)
        run_ddi_analysis(input$selected_drug_A, input$selected_drug_B)
    }, ignoreNULL = TRUE)
        
    observeEvent(input$update_network_button, {
        req(full_intersect_data(), input$top_n_genes, current_drug_a(), current_drug_b())
        
        if (nrow(full_intersect_data()) == 0) {
           shinyalert::shinyalert("Info", "No gene data available to update network.", type = "info")
           return()
        }
        
        show_modal_spinner(spin = "circle", color = "#4158D0", text = "Updating network graph...")
        
        top_n_val <- input$top_n_genes
        if (is.null(top_n_val) || top_n_val < 1) {
            top_n_val <- 10
        }
        
        updated_network_data <- generate_network_graph_data(
            intersect_data = full_intersect_data(), 
            top_n_val = top_n_val, 
            drug_a_name = current_drug_a(),
            drug_b_name = current_drug_b()
        )
        
        session$sendCustomMessage(type = 'echarts_data', updated_network_data)
        remove_modal_spinner()
        
    }, ignoreNULL = TRUE)

    observeEvent(input$download_gene_csv, {
      req(full_table_data_for_download(), current_drug_a(), current_drug_b())
      
      filename <- paste0(current_drug_a(), "_", current_drug_b(), "_Genes.csv")
      
      download_data <- full_table_data_for_download()
      download_data$Gene <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$Gene)
      
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
    
    observeEvent(input$download_gene_excel, {
      req(full_table_data_for_download(), current_drug_a(), current_drug_b())
      
      filename <- paste0(current_drug_a(), "_", current_drug_b(), "_Genes.xlsx")
      
      download_data <- full_table_data_for_download()
      download_data$Gene <- gsub('<a[^>]*>([^<]+)</a>', '\\1', download_data$Gene)
      
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

    output$drugA_info <- renderUI({ div(style="padding: 20px; text-align: center; color: grey;", "Select Drug A and Drug B, then click Analyze") })
    output$drugB_info <- renderUI({ div(style="padding: 20px; text-align: center; color: grey;", "") })
    output$gene_matrix <- renderDT({ datatable(data.frame(Message="Please select two different drugs and click Analyze to view interacting genes."), options=list(dom='t')) })
    output$ddi_info_ddinter <- renderUI({ div(style="padding: 10px; color: grey;", "Interaction details from DDInter will appear here after analysis.") })
    output$ddi_info_mecddi <- renderUI({ div(style="padding: 10px; color: grey;", "Interaction details from MecDDI will appear here after analysis.") })
    output$ddi_info_rxnav <- renderUI({ div(style="padding: 10px; color: grey;", "Interaction details from RxNav will appear here after analysis.") })
    
    observe({
        req(!initial_echart_cleared())
        session$sendCustomMessage(type = 'echarts_data', list(nodes=list(), links=list(), categories=list()))
        initial_echart_cleared(TRUE)
    }, priority = 5)

  })
}