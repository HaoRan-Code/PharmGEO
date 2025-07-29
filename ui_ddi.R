ddiUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Drug-Drug Interaction",
    icon  = icon("exchange-alt", lib = "font-awesome"),
    tags$div(
      style = "width: 90%; margin: 0 auto;",
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
            "The Drug-Drug Interaction section enables users to explore and analyze interactions between different pharmaceutical compounds. Users can select two drugs to view their detailed information, intersecting genes, and interaction details from multiple databases. Additionally, an interaction network visualization provides insights into the relationships between the selected drugs and associated genes."
          )
        )
      )
    )),
    fluidPage(
      tags$head(
        tags$style(HTML("\n          .ddi-table tbody td {\n            white-space: normal !important;\n            word-wrap: break-word;\n            vertical-align: top !important;\n          }\n          .ddi-table {
            width: 100% !important;
            table-layout: auto !important;
          }
          .ddi-table td:first-child, 
          .ddi-table td:not(:first-child) {
            width: auto !important;
          }
          .dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody table.ddi-table {
            table-layout: auto !important;
            width: 100% !important;
          }
          .dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody table.ddi-table td:first-child {
            width: auto !important;
          }
          .dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody table.ddi-table td:not(:first-child) {
            width: auto !important;
          }
        "))
      ),
      tags$div(
        style = "width: 90%; margin: 0 auto;",
        fluidRow(
          box(
            title = strong("Select Drugs"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "padding: 25px; background: linear-gradient(135deg, rgba(65, 88, 208, 0.05) 0%, rgba(200, 80, 192, 0.05) 100%); border-radius: 16px;",
              fluidRow(
                column(5,
                  div(
                    style = "background: linear-gradient(135deg, rgba(65, 150, 208, 0.15) 0%, rgba(100, 180, 255, 0.15) 100%); padding: 10px; border-radius: 8px;",
                    pickerInput(
                      inputId = ns("selected_drug_A"), 
                      label = h5(tags$b("Select Drug A:")),
                      choices = NULL,
                      selected = NULL,
                      options = list(
                        `live-search` = TRUE, 
                        placeholder = "Please select a drug",
                        container = "body",
                        dropupAuto = FALSE,
                        virtualScroll = TRUE,
                        maxOptions = 50,
                        liveSearchNormalize = TRUE
                      )
                    )
                  )
                ),
                column(5,
                  div(
                    style = "background: linear-gradient(135deg, rgba(200, 80, 192, 0.15) 0%, rgba(255, 100, 100, 0.15) 100%); padding: 10px; border-radius: 8px;",
                    uiOutput(ns("drugB_picker"))
                  )
                ),
                column(2,
                  div(
                    style = "margin-top: 28px;",
                    actionButton(
                      inputId = ns("go_button"), 
                      label = "Analyze", 
                      icon = icon("play"), 
                      class = "btn btn-primary btn-block",
                      style = "color: white; font-weight: bold;"
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Drug information display with dynamic titles
        fluidRow(
          box(
            title       = strong("Drug Information"),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            div(
              style = "padding: 20px;",
              # Dynamic Drug A title
              uiOutput(ns("drugA_title")),
              uiOutput(ns("drugA_info")),
              hr(style = "margin: 30px 0; border-color: var(--border-light);"),
              # Dynamic Drug B title
              uiOutput(ns("drugB_title")),
              uiOutput(ns("drugB_info"))
            )
          )
        ),
        
        # Gene matrix with dynamic title
        fluidRow(
          box(
            title = uiOutput(ns("gene_matrix_title")),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(style = "padding: 20px;", DTOutput(ns("gene_matrix")))
          )
        ),
        
        # DDI information with dynamic title
        fluidRow(
          box(
            title = uiOutput(ns("ddi_details_title")),
            status      = "primary",
            solidHeader = TRUE,
            width       = 12,
            div(
              style = "padding: 20px; height: 600px; overflow:auto;",
              tabsetPanel(
                id = ns("ddi_tabs"),
                type = "tabs",
                tabPanel("DDInter", div(uiOutput(ns("ddi_info_ddinter")))),
                tabPanel("MecDDI", div(uiOutput(ns("ddi_info_mecddi")))),
                tabPanel("RxNav", div(uiOutput(ns("ddi_info_rxnav"))))
              )
            )
          )
        ),
        
        # Interaction network with dynamic title
        fluidRow(
          box(
            title = uiOutput(ns("network_title")),
            status = "primary", solidHeader = TRUE, width = 12,
            div(
              style = "padding: 20px;", 
                              # --- 控制行 --- 
              fluidRow(
                  column(2, offset = 4, style = "padding-right: 5px;",
                     numericInput(ns("top_n_genes"), label = "Top N Genes per Category:", 
                                  value = 10, min = 1, step = 5, width = '200px')
                 ),
                  column(2, style = "padding-left: 5px; margin-top: 28px;",
                     actionButton(ns("update_network_button"), label = "Update Network", 
                                  icon = icon("sync-alt"), class = "btn-primary",
                                 style="color: white !important;") 
                 )
              ),
               # --- 图表区域 --- 
              fluidRow(
                 column(12, 
                    tags$div(id = "echart_container", 
                              style = "width:100%; height:600px; border-radius: 16px; overflow: hidden; border: 1px solid var(--border-light);"),
                    tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/echarts@5/dist/echarts.min.js")),
                                        tags$script(HTML("// ECharts处理已移动到外部JavaScript文件"))
                 ) 
              )
            )
          )
        )
      )
    )
  ) 
} 