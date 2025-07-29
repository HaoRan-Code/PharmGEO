drugInformationUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Drug Information",
    icon  = icon("vial", lib = "font-awesome"),
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
            "The Drug Information section provides details on specific drugs, including molecular properties, descriptions, and related genes. It also lists genes consistently found across datasets to identify key gene-drug interactions."
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        div(
          style = "background: white; padding: 25px; border-radius: 16px; margin-bottom: 25px; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.05);",
          pickerInput(
            inputId  = ns("selected_drug"),
            label    = h5(tags$b("Select a Drug:")),
            choices  = NULL,
            selected = NULL,
            options  = list(
              `live-search` = TRUE, 
              placeholder = "Please select a drug",
              container = "body",
              dropupAuto = FALSE
            )
          ),
          div(
            style = "text-align: center; margin-top: 25px;",
            actionButton(
              inputId = ns("drug_onto_submit"),
              label   = "Analyze", 
              icon    = icon("play"), 
              class   = "btn btn-primary btn-block",
              style   = "color: white; font-weight: bold;"
            )
          )
        )
      ),
      column(
        width = 9,
        fluidRow(
          box(
            title       = strong("Drug Information"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            div(style = "padding: 20px; min-height: 300px;", uiOutput(ns("drugInfo")))
          )
        ),
        fluidRow(
          box(
            title       = strong("Highly Consistent Genes"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            style       = 'overflow-y: auto; margin-bottom: 25px;',
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
                  inputId = ns("help_genes"),
                  label = "View Column Definitions",
                  icon = icon("circle-info", lib = "font-awesome"),
                  class = "btn btn-outline-primary btn-sm",
                  style = "font-size: 12px; padding: 4px 12px;"
                )
              )
            ),
            DTOutput(ns("drug_gene_table"))
          )
        )
      )
    )
  )
} 