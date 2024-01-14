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

  selected <- reactiveValues(
    rows = seq.int(nrow(golem::get_golem_options("abundance"))),
    cols = seq.int(ncol(golem::get_golem_options("abundance")))
  )
  observeEvent(input$feature_table_rows_all, {
    selected$rows <- input$feature_table_rows_all
  })
  observeEvent(input$sample_table_rows_all, {
    selected$cols <- input$sample_table_rows_all
  })

  observeEvent(sample_anno,{
    freezeReactiveValue(input, "column_split")
    updateSelectInput(session, "column_split",
                      choices = c("None", colnames(sample_anno())))
  })
  observeEvent(feature_anno,{
               freezeReactiveValue(input, "row_split")
               updateSelectInput(
                 session, "row_split",
                 choices = c("None", colnames(feature_anno())))
  })

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

  output$heatmap <- renderPlot({
    req(abundance, selected)
    checkmate::assert_matrix(abundance())
    # remove rows with only missing values
    m <- abundance()[selected$rows,
                     selected$cols,
                     drop = FALSE]
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
    # cluster rows
    row_cl <- hclust(dist(impute::impute.knn(m)$data))
    # heatmap
    ComplexHeatmap::Heatmap(
      matrix = m,
      top_annotation = column_ha,
      right_annotation = row_ha,
      cluster_columns = FALSE,
      cluster_rows = row_cl,
      column_split = column_split,
      row_split = row_split
    )
  })
}
