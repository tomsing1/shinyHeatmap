# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "shinyHeatmap",
  pkg_title = "A shiny app to create & modify a ComplexHeatmap",
  pkg_description = paste(
    "This shiny web application demonstrates how to create & modify a",
    "a ComplexHeatmap, allowing users to specify e.g. the grouping and order",
    "of columns by interacting with a data table."),
  author_first_name = "Thomas",
  author_last_name = "Sandmann",
  author_email = "tomsing1@gmail.com", # Your Email
  repo_url = "https://github.com/tomsing1/shinyHeatmap",
  pkg_version = "0.0.0.9000" # The Version of the package containing the App
)

## Set {golem} options ----
golem::set_golem_options()

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
## See ?usethis for more information
usethis::use_apache_license()
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
