#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @import ComplexHeatmap
#' @import DT
#' @noRd
app_server <- function(input, output, session) {

  abundance <- reactiveVal(golem::get_golem_options("abundance"))
  feature_anno <- reactiveVal(golem::get_golem_options("feature_anno"))
  sample_anno <- reactiveVal(golem::get_golem_options("sample_anno"))

  observeEvent(sample_anno,{
    updateSelectInput(session, "column_split",
                      choices = c("None", colnames(sample_anno())))
  })
  observeEvent(feature_anno,
               updateSelectInput(
                 session, "row_split",
                 choices = c("None", colnames(feature_anno())))
  )

  output$sample_table <- DT::renderDT({
    checkmate::assert_data_frame(sample_anno())
    data.frame(
      ID = factor(row.names(sample_anno())),
      sample_anno()
    ) |>
      DT::datatable(
        fillContainer = TRUE,
        filter = 'top',
        rownames = FALSE,
        selection = list(mode = 'none', target = 'row')
      )
  })

  output$feature_table <- DT::renderDT({
    checkmate::assert_data_frame(feature_anno())
    data.frame(
      ID = factor(row.names(feature_anno())),
      feature_anno()
    ) |>
      DT::datatable(
        fillContainer = TRUE,
        rownames = FALSE,
        filter = 'top',
        selection = list(mode = 'none', target = 'row')
      )
  })

  rows <- reactive({
    if(is.null(input$feature_table_rows_all)) {
      rows <- seq.int(nrow(abundance()))
    } else {
      rows <- input$feature_table_rows_all
    }
    return(rows)
  })

  cols <- reactive({
    if(is.null(input$sample_table_rows_all)) {
      cols <- seq.int(ncol(abundance()))
    } else {
      cols <- input$sample_table_rows_all
    }
    return(cols)
  })

  output$heatmap <- renderPlot({
    req(abundance)
    checkmate::assert_matrix(abundance())

    # remove rows with only missing values
    m <- abundance()[rows(), cols(), drop = FALSE]
    m <- m[!apply(m, 1, \(x) all(is.na(x))), , drop = FALSE]

    # annotations
    column_ha <- ComplexHeatmap::HeatmapAnnotation(
      df = sample_anno()[colnames(m), , drop = FALSE]
    )
    row_ha <- ComplexHeatmap::rowAnnotation(
      df = feature_anno()[row.names(m), , drop = FALSE]
    )
    # column split
    if (input$column_split != "None") {
      column_split <- sample_anno()[colnames(m), input$column_split]
    } else {
      column_split <- NULL
    }
    if (input$row_split != "None") {
      row_split <- sample_anno()[colnames(m), input$row_split]
    } else {
      row_split <- NULL
    }
    # heatmap
    ComplexHeatmap::Heatmap(
      matrix = m,
      top_annotation = column_ha,
      right_annotation = row_ha,
      cluster_columns = FALSE,
      column_split = column_split,
      row_split = row_split
    )
  })
}
