#' Create a printable table of the growth model fixed-effects correlation matrix (mixed-effects models ONLY)
#'
#' @description
#' 'growth_model_fixed_cor_table()' creates a flextable object that can be used for documentation or Rmarkdown reports from the list object created by \code{\link{summarize_growth_model}}.
#' The 'model_fixed_corr' data frame from the list object for mixed-effects models is used to generate the table.
#'
#'
#' @inheritParams growth_model_summary_table
#'
#' @return A flextable object of the 'model_fixed_corr' data frame
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}} \code{\link[flextable]{flextable}}
#' @import flextable
#' @importFrom knitr knit_print
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(data_frame = exp_mixed_data, function_type = "exponential")
#' # Summarize the data by creating a summary list object
#' exp_mixed_model_summary <- summarize_growth_model(data_frame = exp_mixed_data,
#' growth_model_object = exp_mixed_model,
#' model_type = "mixed",
#' function_type = "exponential")
#' # Create flextable object of the growth model fixed-effects correlation matrix ('model_fixed_corr' data frame from summary list object) for documentation
#' exp_model_cor_table <- growth_model_summary_table(growth_model_summary_list = exp_mixed_model_summary)
#' print(exp_model_cor_table)
growth_model_fixed_cor_table <- function(growth_model_summary_list,
                                         use_knit_print = FALSE) {
  # Check function inputs
  stopifnot(
    is.list(growth_model_summary_list),
    exists("model_fixed_corr", growth_model_summary_list),
    is.logical(use_knit_print)
  )

  # Extract model_summary_long from growth_model_summary_list object
  data_frame <- growth_model_summary_list[["model_fixed_corr"]]

  # Round coefficients by two decimals
  data_corr <- data_frame %>%
    dplyr::mutate_if(is.numeric, ~ round(., 2))
  # Create a flex table object of correlation table
  flx_tbl_corr <- flextable(data_corr) %>%
    flextable::hline_top(
      border = flextable::fp_border_default(width = 0),
      part = "header"
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(
      align = "center", part = "body",
      j = c(2:ncol(data_corr))
    ) %>%
    flextable::align(align = "left", part = "body", j = 1) %>%
    flextable::fontsize(size = 12, part = "header") %>%
    flextable::font(fontname = "Albany AMT", part = "body") %>%
    flextable::font(fontname = "Albany AMT", part = "header") %>%
    flextable::bold(part = "header", bold = TRUE) %>%
    flextable::bold(part = "body", j = 1, bold = TRUE) %>%
    flextable::compose(
      i = 1, j = 1,
      value = flextable::as_paragraph(as_chunk(" ")),
      part = "header"
    ) %>%
    flextable::fontsize(size = 12, part = "body") %>%
    flextable::autofit(part = "all")
  if (use_knit_print == TRUE) {
    knitr::knit_print(flx_tbl_corr)
  } else {
    return(flx_tbl_corr)
  }
}
