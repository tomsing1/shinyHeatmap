#' Retrieve target LC/MS data published by Xia et al, 2022
#' @param contrast Scalar character, the comparison of interest
#' @importFrom readxl read_excel
#' @importFrom curl has_internet
#' @export
#' @return A list with four elements:
#' - abundance: a numeric feature x sample matrix with 151 rows and 18 columns
#' - stats: a `data.frame` with 145 rows and 6 columns
#' - sample_anno: a `data.frame` with 18 rows and 4 columns
#' - feature_anno: a `data.frame` with 151 rows and 0 columns
#' @source [Xia et al, Molecular neurodegeneration, 2022](https://molecularneurodegeneration.biomedcentral.com/articles/10.1186/s13024-022-00547-7)
#' @examples
#' # retrieve the four different objects
#' dl <- xia()
#' sapply(dl, dim)
#' sapply(dl, \(x) class(x)[1])
xia <- function(contrast = c("1. APP-SAA heterozygo KI vs WT",
                             "2. APP-SAA homozygous KI vs WT",
                             "3. Hom_vs_Het")) {
  if (!curl::has_internet()) {
    stop("An internet connection is required to download these data.")
  }
  contrast <- match.arg(contrast)
  urls <- c(
    "abundance" = paste0(
      "https://static-content.springer.com/esm/art%3A10.1186%2",
      "Fs13024-022-00547",
      "-7/MediaObjects/13024_2022_547_MOESM13_ESM.xlsx"),
    "stats" = paste0(
      "https://static-content.springer.com/esm/art%3A10.1186%",
      "2Fs13024-022-00547",
      "-7/MediaObjects/13024_2022_547_MOESM14_ESM.xlsx"
    )
  )

  # retrieve the first supplementary data file
  tmp_file <- tempfile(fileext = ".xlsx")
  download.file(urls["abundance"], tmp_file, quiet = TRUE)
  # parse sample- and feature-annotations, and normalized abundances
  sample_anno <- readxl::read_excel(tmp_file, sheet = "sample_annotations") |>
    as.data.frame()
  row.names(sample_anno) <- sample_anno$sample_id
  sample_anno <- .cleanup_annotations(sample_anno)
  sample_anno$sample_id <- NULL

  feature_anno <- readxl::read_excel(tmp_file, sheet = "feature_annotations") |>
    as.data.frame()
  feature_anno <- feature_anno[, grep("QTRAP|XEVO|m.z", colnames(feature_anno),
                                      invert = TRUE)]
  feature_anno <- feature_anno[!duplicated(feature_anno$component_name), ]
  row.names(feature_anno) <- feature_anno$component_name
  feature_anno$feature_id <- feature_anno$component_name <- NULL
  feature_anno <- feature_anno[!feature_anno$is_internal_standard,]
  feature_anno <- feature_anno[, setdiff(
    colnames(feature_anno), c("is_internal_standard")),
    drop = FALSE]
  feature_anno <- .cleanup_annotations(feature_anno)
  abundance <- readxl::read_excel(tmp_file, sheet = "peak_area_ratio_to_is") |>
    as.data.frame()
  m <- data.matrix(
    abundance[, grep("^LA", colnames(abundance), value = TRUE)]
  )
  row.names(m) <- abundance$component_name

  # retrieve the second supplementary file with statistical results for
  # the comparison of interest.
  download.file(urls["stats"], tmp_file, quiet = TRUE)
  stats <- readxl::read_excel(tmp_file, sheet = contrast) |>
    as.data.frame()
  row.names(stats) <- stats$feature_id
  stats$feature_id <- NULL
  stats <- .cleanup_annotations(stats)
  return(list(abundance = m,
              stats = stats,
              sample_anno = sample_anno,
              feature_anno = feature_anno))
}

# @importFrom utils type.convert
.cleanup_annotations <- function(df, min_levels = 2L, max_levels = 6L,
                                 min_numeric = 3L) {
  df <- utils::type.convert(df, as.is = FALSE)
  for (col in colnames(df)) {
    n_unique <- length(unique(df[[col]]))
    if (n_unique < min_numeric) {
      # convert numeric columns with fewer than `min_numeric` values into factor
      df[[col]] <- factor(df[[col]])
    }
    # remove any columns with fewer than `min_levels` different values
    if (n_unique < min_levels) {
      df <- df[, setdiff(colnames(df), col), drop = FALSE]
    # remove factors with more than `max_levels`
    } else if (is.factor(df[[col]])) {
      if (n_unique < min_levels || n_unique > max_levels) {
        df <- df[, setdiff(colnames(df), col), drop = FALSE]
      }
    }
  }
  return(df)
}
