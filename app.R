renv::load()
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(shinybusy)
library(shinyalert)
library(RSQLite)
library(shinyjs)
library(DT)
library(pool)
library(DBI)
library(dplyr)

global_data_cache <- new.env()
global_data_cache$loaded_data <- list()
global_data_cache$lock <- FALSE

load_data_safe <- function(data_name, session_id = NULL) {
  cache_key <- if (!is.null(session_id)) {
    paste(data_name, session_id, sep = "_")
  } else {
    data_name
  }
  
  if (data_name %in% names(global_data_cache$loaded_data)) {
    return(global_data_cache$loaded_data[[data_name]])
  }
  
  while (global_data_cache$lock) {
    Sys.sleep(0.01)
  }
  
  if (data_name %in% names(global_data_cache$loaded_data)) {
    return(global_data_cache$loaded_data[[data_name]])
  }
  
  global_data_cache$lock <- TRUE
  on.exit(global_data_cache$lock <- FALSE)
  
  cat("Loading", data_name, "...\n")
  switch(data_name,
    "dd_interaction" = {
      load("./supple/infor_table.Rdata")
      global_data_cache$loaded_data[[data_name]] <- dd_interaction
    },
    "infor_matrix" = {
      load("./supple/infor_table_all.Rdata")
      global_data_cache$loaded_data[[data_name]] <- infor_matrix
    },
    "atc_drug_gmcs_cv" = {
      load("./supple/atc_drug_gmcs_cv.Rdata")
      global_data_cache$loaded_data[[data_name]] <- atc_drug_gmcs_cv
    },
    "drug_info" = {
      load("./supple/drug_info.Rdata")
      global_data_cache$loaded_data[[data_name]] <- drug_info
    },
    "drug_names" = {
      load("./supple/drug_names.Rdata")
      global_data_cache$loaded_data[[data_name]] <- drug_names
    },
    "drug_choices" = {
      load("./supple/Drug_ontology_drug_choices.Rdata")
      global_data_cache$loaded_data[[data_name]] <- drug_choices
    },
    "gene_choices" = {
      load("./supple/gene_choices.Rdata")
      global_data_cache$loaded_data[[data_name]] <- gene_choices
    }
  )
  
  return(global_data_cache$loaded_data[[data_name]])
}

help_content_static <- tryCatch({
  help_html_path <- "www/help_documentation.html"
  if (file.exists(help_html_path)) {
    includeHTML(help_html_path)
  } else {
    div(style = "text-align: center; padding: 50px; color: #757575;",
        icon("exclamation-triangle", style = "font-size: 48px; margin-bottom: 20px;"),
        h3("Help documentation HTML not found"))
  }
}, error = function(e) {
  div(style = "text-align: center; padding: 50px; color: #f44336;",
      h3("Error loading help content"))
})

source("ui_rnaseq_datasets.R")
source("server_rnaseq_datasets.R")
source("ui_drug_information.R")
source("server_drug_information.R")
source("ui_gene_information.R")
source("server_gene_information.R")
source("ui_ddi.R")
source("server_ddi.R")
source("./supple/useShinydashboardPlus.R")

hot_genes <- c(
  "TP53", "EGFR", "KRAS", "BRCA1", "BRCA2", 
  "ALK", "BRAF", "PIK3CA", "MYC", "PTEN",
  "RB1", "APC", "ERBB2", "FLT3", "PDGFRA",
  "KIT", "IDH1", "IDH2", "RET", "NOTCH1"
)

db_paths <- list(
  diffsig            = "supple/diffsig_data.sqlite",
  analysis_results   = "supple/analysis_results.sqlite", 
  drug_gene          = "supple/drug_gene_database.sqlite",
  drug_interactions  = "supple/drug_interactions.db",
  gene_info         = "supple/gene_info_database.sqlite"
)

db_pools <- list()

get_db_pool <- function(db_name) {
  if (is.null(db_pools[[db_name]])) {
    if (file.exists(db_paths[[db_name]])) {
      cat("Creating connection pool for", db_name, "\n")
      db_pools[[db_name]] <<- pool::dbPool(
        drv = SQLite(),
        dbname = db_paths[[db_name]],
        maxSize = 10,
        minSize = 2,
        idleTimeout = 3600000
      )
    } else {
      warning("Database file not found: ", db_paths[[db_name]])
      return(NULL)
    }
  }
  return(db_pools[[db_name]])
}

connect_to_db <- function(db_name) {
  pool <- get_db_pool(db_name)
  if (is.null(pool)) {
    return(NULL)
  }
  return(pool)
}

safe_db_query <- function(db_name, query, params = list()) {
  tryCatch({
    pool <- connect_to_db(db_name)
    if (is.null(pool)) {
      return(NULL)
    }
    
    if (length(params) > 0) {
      result <- pool::dbGetQuery(pool, query, params = params)
    } else {
      result <- pool::dbGetQuery(pool, query)
    }
    return(result)
  }, error = function(e) {
    cat("Database query error:", e$message, "\n")
    return(NULL)
  })
}

cleanup_global_resources <- function() {
  cat("Cleaning up global resources...\n")
  for (pool_name in names(db_pools)) {
    if (!is.null(db_pools[[pool_name]])) {
      tryCatch({
        pool::poolClose(db_pools[[pool_name]])
        cat("Closed pool:", pool_name, "\n")
      }, error = function(e) {
        cat("Error closing pool", pool_name, ":", e$message, "\n")
      })
    }
  }
  db_pools <<- list()
  
  tryCatch({
    gc()
  }, error = function(e) {
    cat("Error during garbage collection:", e$message, "\n")
  })
}

process_in_chunks <- function(data, chunk_size = 100, process_fn, callback_fn = NULL) {
  n <- nrow(data)
  chunks <- ceiling(n / chunk_size)
  
  for (i in 1:chunks) {
    start_idx <- (i-1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n)
    
    chunk_data <- data[start_idx:end_idx, ]
    result <- process_fn(chunk_data)
    
    invalidateLater(0)
    
    if (!is.null(callback_fn)) {
      callback_fn(i, chunks, result)
    }
  }
}

COLORS <- list(
  primary = "#4158D0",
  secondary = "#2196F3", 
  success = "#00b686",
  info = "#42a5f5",
  warning = "#ff9800",
  danger = "#f44336"
)

CARD_STYLE <- "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); margin-bottom: 20px;"
METRIC_GRADIENT_STYLE <- "background: linear-gradient(135deg, #4158D0 0%, #5d73e0 100%); color: white; text-align: center;"

create_info_card <- function(title, content, icon_name = "info-circle", color = COLORS$primary) {
  div(class = "info-card", style = paste0(CARD_STYLE, "border-left: 4px solid ", color, ";"),
      div(style = "display: flex; align-items: center; margin-bottom: 15px;",
          icon(icon_name, style = paste0("color: ", color, "; margin-right: 10px; font-size: 18px;")),
          h5(title, style = paste0("margin: 0; color: ", color, "; font-weight: 600;"))),
      content)
}

ui <- navbarPage(
  id = "Main",
  tags$head(
    tags$link(rel = "icon", type = "image/jpg", href = "logo_webhead.png"),
    HTML("<title>PharmGEO</title>"),
    tags$style(HTML("
      .selectize-dropdown, .dropdown-menu { z-index: 9999 !important; }
      .box { overflow: visible !important; }
      .box-body, .shiny-input-container, .form-group { overflow: visible !important; }
    ")),
    tags$script(src = "app-scripts.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  theme = bs_theme(
    version = 4,
    primary = COLORS$primary,
    secondary = COLORS$secondary,
    success = COLORS$success,
    info = COLORS$info,
    warning = COLORS$warning,
    danger = COLORS$danger,
    base_font = c("'Segoe UI'", "'Roboto'", "sans-serif"),
    font_scale = 1
  ),
  fluid = TRUE,
  title = div(
    style = "display: flex; align-items: center; margin-bottom: 0; padding: 5px 0;",
    img(src = "logo_webhead.png", height = 75, width = 75, style = "margin-right: 15px;"),
    div(
      h5(strong("PharmGEO"), class = "gradient-header", style = "font-size: 24px; margin: 0;"),
      h6("Pharmaco-transcriptomic datasets from GEO", style = "font-size: 14px; margin: 0; color: #757575;")
    )
  ),
  useShinydashboardPlus(),
  useShinyjs(),
  
  nav_panel(
    title = "Home",
    icon  = icon("house", lib = "font-awesome"),
    fluidRow(
      column(width = 10, offset = 1,
        div(style = "padding: 30px 0;",
          box(icon = icon("database"), width = NULL, status = "primary", solidHeader = TRUE,
              title = span("Welcome to ", span("PharmGEO", class = "gradient-header", style = "color: white !important; font-weight: 700;")),
              div(style = "text-align: left; padding: 20px 15px;",
                  tags$p(style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 15px;",
                         "PharmGEO is a comprehensive, manually curated, and quality-controlled platform that systematically integrates pharmaco-transcriptomic data from the GEO database."))),
          
          fluidRow(style = "margin: 30px 0;",
            column(width = 3, div(class = "feature-card metric-card", style = METRIC_GRADIENT_STYLE,
                                  div(style = "margin-bottom: 15px;", icon("chart-bar", style = "font-size: 36px;")),
                                  h2("7,931", style = "margin: 10px 0 5px 0; font-weight: 700;"),
                                  p("Pharmaco-transcriptomics Datasets", style = "margin: 0; font-size: 0.9rem; opacity: 0.9;"))),
            column(width = 3, div(class = "feature-card metric-card", style = "background: linear-gradient(135deg, #2196F3 0%, #42A5F5 100%); color: white; text-align: center;",
                                  div(style = "margin-bottom: 15px;", icon("pills", style = "font-size: 36px;")),
                                  h2("1,334", style = "margin: 10px 0 5px 0; font-weight: 700;"),
                                  p("Unique Drugs with Metadata", style = "margin: 0; font-size: 0.9rem; opacity: 0.9;"))),
            column(width = 3, div(class = "feature-card metric-card", style = "background: linear-gradient(135deg, #2196F3 0%, #42A5F5 100%); color: white; text-align: center;",
                                  div(style = "margin-bottom: 15px;", icon("dna", style = "font-size: 36px;")),
                                  h2("17,776", style = "margin: 10px 0 5px 0; font-weight: 700;"),
                                  p("Average Genes per Dataset", style = "margin: 0; font-size: 0.9rem; opacity: 0.9;"))),
            column(width = 3, div(class = "feature-card metric-card", style = METRIC_GRADIENT_STYLE,
                                  div(style = "margin-bottom: 15px;", icon("project-diagram", style = "font-size: 36px;")),
                                  h2("115,264", style = "margin: 10px 0 5px 0; font-weight: 700;"),
                                  p("Drug-Drug Interactions", style = "margin: 0; font-size: 0.9rem; opacity: 0.9;")))),
          
          fluidRow(style = "margin-top: 40px;",
            column(width = 6, div(class = "feature-card", style = "height: 280px; cursor: pointer; transition: all 0.3s;",
                                  `data-tab` = "RNA-seq Datasets",
                                  div(class = "feature-icon", icon("table", style = "font-size: 32px; color: #4158D0;", lib = "font-awesome")),
                                  h4("RNA-seq Datasets", style = "margin: 20px 0 15px 0; color: var(--text-primary); font-weight: 600;"),
                                  div(style = "text-align: left; padding: 0 20px;",
                                      tags$ul(style = "margin: 0; color: var(--text-secondary); line-height: 1.6;",
                                        tags$li("Access 7,931 curated datasets"),
                                        tags$li("Standardized metadata annotations"),
                                        tags$li("Differential expression analysis"),
                                        tags$li("Pathway enrichment results")
                                      )
                                  )
                                )),
            column(width = 6, div(class = "feature-card", style = "height: 280px; cursor: pointer; transition: all 0.3s;",
                                  `data-tab` = "Drug Information",
                                  div(class = "feature-icon", icon("pills", style = "font-size: 32px; color: #2196F3;", lib = "font-awesome")),
                                  h4("Drug Information", style = "margin: 20px 0 15px 0; color: var(--text-primary); font-weight: 600;"),
                                  div(style = "text-align: left; padding: 0 20px;",
                                      tags$ul(style = "margin: 0; color: var(--text-secondary); line-height: 1.6;",
                                        tags$li("1,334 drugs with detailed profiles"),
                                        tags$li("High-consistency gene signatures"),
                                        tags$li("GMCS quality metrics"),
                                        tags$li("PubChem integration"),
                                        tags$li("Literature evidence support")
                                      )
                                  )
                                ))
                             ),
          
          fluidRow(style = "margin-top: 25px;",
            column(width = 6, div(class = "feature-card", style = "height: 280px; cursor: pointer; transition: all 0.3s;",
                                  `data-tab` = "Gene Information",
                                  div(class = "feature-icon", icon("dna", style = "font-size: 32px; color: #1976D2;", lib = "font-awesome")),
                                  h4("Gene Information", style = "margin: 20px 0 15px 0; color: var(--text-primary); font-weight: 600;"),
                                  div(style = "text-align: left; padding: 0 20px;",
                                      tags$ul(style = "margin: 0; color: var(--text-secondary); line-height: 1.6;",
                                        tags$li("Gene-centric drug associations"),
                                        tags$li("Cross-dataset validation"),
                                        tags$li("GeneCards integration")
                                      )
                                  )
                                )),
            column(width = 6, div(class = "feature-card", style = "height: 280px; cursor: pointer; transition: all 0.3s;",
                                  `data-tab` = "Drug-Drug Interaction",
                                  div(class = "feature-icon", icon("project-diagram", style = "font-size: 32px; color: #0D47A1;", lib = "font-awesome")),
                                  h4("Drug-Drug Interactions", style = "margin: 20px 0 15px 0; color: var(--text-primary); font-weight: 600;"),
                                  div(style = "text-align: left; padding: 0 20px;",
                                      tags$ul(style = "margin: 0; color: var(--text-secondary); line-height: 1.6;",
                                        tags$li("119,298 interaction records"),
                                        tags$li("Transcriptomic-level evidence"),
                                        tags$li("Multi-source integration"),
                                        tags$li("Network visualization")
                                      )
                                  )
                                ))
                             )
        )
      )
    )
  ),
  
  rnaseqDatasetsUI("rnaseq"),
  
  drugInformationUI("drugInfo"),
  
  geneInformationUI("geneInfo"),
  
  ddiUI("ddi"),
  
  nav_panel(
    title = "Download",
    icon  = icon("download", lib = "font-awesome"),
    fluidRow(
      column(width = 10, offset = 1,
        box(title = strong("Available Downloads"), status = "primary", solidHeader = TRUE, width = 12,
          div(style = "padding: 30px;",
            fluidRow(
              column(width = 6, div(class = "feature-card",
                                    div(class = "feature-icon", icon("file-excel", style = "font-size: 28px; color: #2E7D32;", lib = "font-awesome")),
                                    h4("Annotated Drug Metadata", style = "margin-top: 0; color: var(--text-primary); font-weight: 600;"),
                                    p(style = "text-align: center; color: var(--text-secondary);", "Download curated drug annotation metadata (Excel format)."),
                                    div(style = "margin-top: 15px;",
                                        downloadButton("download_metadata", "Download Metadata", class = "btn btn-success", style = "color: white; font-weight: bold;")))),
              column(width = 6, div(class = "feature-card",
                                    div(class = "feature-icon", icon("file-alt", style = "font-size: 28px; color: #42a5f5;", lib = "font-awesome")),
                                    h4("Differential Gene Lists", style = "margin-top: 0; color: var(--text-primary); font-weight: 600;"),
                                    p(style = "text-align: center; color: var(--text-secondary);", "Download experiment-specific lists of significantly differentially expressed genes (Rdata format)."),
                                    div(style = "margin-top: 15px;",
                                        downloadButton("download_diffsig", "Download Gene Lists", class = "btn btn-info", style = "color: white; font-weight: bold;"))))
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = "Help",
    icon  = icon("question-circle", lib = "font-awesome"),
    div(class = "help-content-container", style = "padding: 40px 60px; font-family: 'Segoe UI', 'Roboto', sans-serif; line-height: 1.6; background: #fafbfc;",
        help_content_static)
  )
)

server <- function(input, output, session) {
  
  session_id <- session$token
  
  session$onSessionEnded(function() {
    cat("Session", session_id, "ended - cleaning up session resources\n")
    gc()
  })
  
  observeEvent(input$Main, {
    panel <- input$Main
    
    if (panel == "RNA-seq Datasets") {
       
    } else if (panel == "Drug Information") {
       
    } else if (panel == "Gene Information") {
       
    } else if (panel == "Drug-Drug Interaction") {
       delay(250, session$sendCustomMessage(type = 'resize_ddi_echart', message = list()))
    } else if (panel == "Download") {
       
    } else if (panel == "Help") {
       
    }
  })
  
  observeEvent(input$navigate_to, {
    req(input$navigate_to)
    updateNavbarPage(session, "Main", selected = input$navigate_to)
  })
  
  rnaseqDatasetsServer("rnaseq", 
                       dd_interaction_data = load_data_safe("dd_interaction", session_id), 
                       infor_matrix_data = load_data_safe("infor_matrix", session_id), 
                       connect_to_db = connect_to_db)

  drugInformationServer("drugInfo", 
                        drug_choices = load_data_safe("drug_choices", session_id), 
                        drug_info_data = load_data_safe("drug_info", session_id), 
                        connect_to_db = connect_to_db)

  geneInformationServer("geneInfo", 
                        gene_choices = load_data_safe("gene_choices", session_id), 
                        hot_genes = hot_genes,
                        connect_to_db = connect_to_db,
                        process_in_chunks = process_in_chunks)

  ddiServer("ddi", connect_to_db = connect_to_db)
  
  output$download_metadata <- downloadHandler(
    filename = function() {
      "GEO_drug_annotation_metadata.xlsx"
    },
    content = function(file) {
      file.copy("download/GEO_drug_annotation_metadata.xlsx", file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$download_diffsig <- downloadHandler(
    filename = function() {
      "all_diffsig_list.Rdata"
    },
    content = function(file) {
      file.copy("download/all_diffsig_list.Rdata", file)
    },
    contentType = "application/octet-stream"
  )
}

onStop(function() {
  cleanup_global_resources()
  cat("Application stopped and global resources cleaned up.\n")  
})

shinyApp(ui = ui, server = server)
