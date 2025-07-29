rnaseqDatasetsUI <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "RNA-seq Datasets",
    tags$style(type = "text/css", ".no-fixed-layout.dataTable, .no-fixed-layout.dataTable td { width: auto !important; table-layout: auto !important; }") ,
    icon  = icon("table", lib = "font-awesome"),
    column(
      8, offset = 2,
    fluidRow(
      box(
        width       = 12,
        status      = "primary",
        solidHeader = TRUE,
        title       = "Introduction",
        div(
          style = "text-align: left; padding: 15px;",
          tags$p(
            style = "line-height: 1.6;",
            "This section presents a curated collection of pharmacotranscriptomic datasets systematically retrieved from the GEO database. You can efficiently browse and select datasets for in-depth analysis, including metadata inspection, differential gene expression profiling, and advanced functional enrichment analyses such as GO, KEGG, and GSEA."
          )
        )
      )
      )
    ),
    fluidRow(
      column(
        8, offset = 2,
        box(
          width       = 12,
          status      = "primary",
          solidHeader = TRUE,
          title       = "Dataset Selection",
          div(
            style = "text-align: center; padding: 20px 15px;",
            tags$div(
              style = "background: linear-gradient(135deg, rgba(65, 88, 208, 0.05) 0%, rgba(200, 80, 192, 0.05) 100%); padding: 20px; border-radius: 16px; margin-bottom: 25px; border-left: 4px solid var(--primary);",
              icon("info-circle", style = "color: var(--primary); margin-right: 10px;"),
              tags$strong(
                "Please select a dataset from the table below, then click",
                tags$span(style = "color: var(--primary); font-weight: bold;", "Analyze"),
                "to proceed."
              )
            ),
            DTOutput(ns("dd_interaction_output")),
            div(
              style = "margin-top: 30px; margin-bottom: 15px;",
              actionButton(
                inputId = ns("drug_gene_submit"),
                label = "Analyze",
                icon = icon("play"),
                class = "btn btn-primary btn-lg",
                style = "color: white; font-weight: bold;"
              )
            )
          )
        )
      )
    ),
    hr(style = "margin: 40px 0; border-color: var(--border-light);"),
    uiOutput(ns("additional_content"))
  )
} 