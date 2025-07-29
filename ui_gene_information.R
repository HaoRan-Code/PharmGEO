geneInformationUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Gene Information",
    icon  = icon("dna", lib = "font-awesome"),
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
            "The Drug-Drug Interaction section analyzes interactions between drugs. Selecting two drugs displays their details, intersecting genes, and interaction data from multiple databases(DDInter, MecDDI, RxNav). A network visualization further illustrates the drug-gene interaction relationships."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        div(
          style = "background: white; padding: 25px; border-radius: 16px; margin-bottom: 25px; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.05);",
          h5(tags$b("Select a Gene:")),
          
          div(
            style = "margin-bottom: 20px;",
            h6(tags$b("Hot Genes:"), icon("fire", style = "color: #ff5252;"), style = "margin-bottom: 10px; color: #4158D0;"),
            div(
              id = ns("hot_gene_links"),
              style = "display: flex; flex-wrap: wrap; gap: 8px;",
              uiOutput(ns("hot_gene_buttons"))
            ),
            tags$style(HTML("
              .hot-gene-link:hover {
                background: linear-gradient(135deg, rgba(65, 88, 208, 0.2) 0%, rgba(200, 80, 192, 0.2) 100%) !important;
                transform: translateY(-2px);
                box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
                text-decoration: none;
                color: #4158D0 !important;
              }
            "))
          ),
          
          div(
            style = "margin-top: 15px;",
            h6(tags$b("All Genes:"), icon("dna", style = "color: #4158D0;"), style = "margin-bottom: 10px; color: #4158D0;"),
            pickerInput(
              inputId = ns("selected_gene"),
              label = NULL,
              choices = NULL,
              selected = NULL,
              options = list(
                `live-search` = TRUE,
                placeholder = "Select any gene",
                container = "body", 
                dropupAuto = FALSE,
                virtualScroll = TRUE,
                maxOptions = 100,
                liveSearchNormalize = TRUE
              )
            )
          ),
          
          div(
            style = "text-align: center; margin-top: 25px;",
            actionButton(
              inputId = ns("gene_onto_submit"),
              label = "Analyze",
              icon = icon("play"), 
              class = "btn btn-primary btn-block",
              style = "color: white; font-weight: bold;"
            )
          )
        )
      ),
      column(
        width = 9,
        fluidRow(
          box(
            title       = strong("Gene Information"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            div(style = "padding: 20px; min-height: 200px;", uiOutput(ns("geneInfo")))
          )
        ),
        fluidRow(
          box(
            title       = strong("Associated Drugs"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            style       = 'overflow-y: auto',
            div(
              style = "padding: 10px 20px; margin-bottom: 15px; background: #f8faff; border-radius: 8px; border-left: 4px solid #4158D0;",
              div(
                style = "display: flex; align-items: center; justify-content: space-between;",
                span(
                  style = "color: #4158D0; font-weight: 500;",
                  icon("info-circle", lib = "font-awesome", style = "margin-right: 8px;"),
                  "Need help understanding the columns?"
                ),
                actionButton(
                  inputId = ns("help_drugs"),
                  label = "View Column Definitions",
                  icon = icon("circle-info", lib = "font-awesome"),
                  class = "btn btn-outline-primary btn-sm",
                  style = "font-size: 12px; padding: 4px 12px;"
                )
              )
            ),
            DTOutput(ns("gene_drug_table"))
          )
        )
      )
    )
  )
} 