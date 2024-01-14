#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @import ComplexHeatmap
#' @import DT
#' @noRd
app_server <- function(input, output, session) {

  abundance <- golem::get_golem_options("abundance")
  feature_anno <- golem::get_golem_options("feature_anno")
  sample_anno <- golem::get_golem_options("sample_anno")

  updateSelectInput(session, "column_split",
                    choices = c("", colnames(sample_anno)),
                    selected = 1)
  updateSelectInput(session, "row_split",
                    choices = c("", colnames(feature_anno)),
                    selected = 1)

  output$sample_table <- DT::renderDT({
    sample_anno |>
      DT::datatable(
        fillContainer = TRUE,
        filter = 'top',
        selection = list(mode = 'none', target = 'row')
      )
  })

  output$feature_table <- DT::renderDT({
    checkmate::assert_data_frame(feature_anno)
    feature_anno |>
      DT::datatable(
        fillContainer = TRUE,
        filter = 'top',
        selection = list(mode = 'none', target = 'row')
      )
  })

  rows <- reactive({
    if(is.null(input$feature_table_rows_all)) {
      rows <- seq.int(nrow(abundance))
    } else {
      rows <- input$feature_table_rows_all
    }
    return(rows)
  })

  cols <- reactive({
    if(is.null(input$sample_table_rows_all)) {
      cols <- seq.int(ncol(abundance))
    } else {
      cols <- input$sample_table_rows_all
    }
    return(cols)
  })

  column_ha = reactive({
    if(ncol(feature_anno) > 0L) {
      req(input$sample_table_rows_all)
      checkmate::assert_data_frame(sample_anno)
      return(
        ComplexHeatmap::HeatmapAnnotation(
          df = sample_anno[cols(), , drop = FALSE]
        )
      )
      } else {
        return(NULL)
      }
  })

  row_ha = reactive({
    if(ncol(feature_anno) > 0L) {
      return(ComplexHeatmap::rowAnnotation(
        df = feature_anno[rows(), , drop = FALSE]
        )
      )
    } else {
      return(NULL)
    }
  })

  output$heatmap <- renderPlot({
    req(abundance)
    checkmate::assert_matrix(abundance)
    ComplexHeatmap::Heatmap(
      matrix = abundance[rows(), cols(), drop = FALSE],
      top_annotation = column_ha(),
      right_annotation = row_ha(),
      cluster_columns = FALSE,
      column_split = sample_anno[[input$column_split]],
      row_split = feature_anno[[input$row_split]]
    )
  })
}
