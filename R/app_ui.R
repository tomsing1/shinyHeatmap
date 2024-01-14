#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @import bslib
#' @importFrom dplyr select_if
#' @noRd
app_ui <- function(request) {

  page_sidebar(
    title = "shinyHeatmap",
    sidebar = sidebar(
      title = "Dataset selection",
      selectInput(
        inputId = "dataset",
        label = "Select dataset",
        choices = "Xia et al",
        selected = 1
      ),
    ),
    layout_column_wrap(
      width = 1/2,
      height = 300,
      card(
        card_header("Heatmap"),
        plotOutput("heatmap"),
        style = "resize:both;",
        full_screen = TRUE
      ),
      navset_card_tab(
        nav_panel(
          "Samples",
          card(
            DT::DTOutput("sample_table")
          )
        ),
        nav_panel(
          "Features",
          card(
            DT::DTOutput("feature_table")
          )
        ),
        nav_panel(
          "Options",
          card(
            selectInput(inputId = "column_split",
                        label = "Split columns by",
                        choices = c("")),
            selectInput(inputId = "row_split",
                        label = "Split rows by",
                        choices = c("")),

          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyHeatmap"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
