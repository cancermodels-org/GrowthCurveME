#' Create a printable table of the summarized growth model results
#'
#' @description
#' 'growth_model_summary_table()' creates a flextable object that can be used for documentation or Rmarkdown reports from the list object created by \code{\link{summarize_growth_model}}.
#' The 'model_summary_long' data frame from the list object is used to generate the table.
#'
#' @inheritParams growth_model_residual_plots
#' @param use_knit_print A logical value to specify whether the flextable should be printer using the \code{\link[knitr]{knit_print}} function instead of the flextable object being returned.
#' Defaults to FALSE.
#'
#' @return A flextable object of the 'model_summary_long' data frame
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}} \code{\link[flextable]{flextable}}
#' @import flextable
#' @importFrom knitr knit_print
#' @importFrom magrittr %>%
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
#' # Create flextable object of the growth model results ('model_summary_long' data frame from summary list object) for documentation
#' exp_model_table <- growth_model_summary_table(growth_model_summary_list = exp_mixed_model_summary)
#' print(exp_model_table)
growth_model_summary_table <- function(growth_model_summary_list,
                                       use_knit_print = FALSE) {
  # Check function inputs
  stopifnot(
    is.list(growth_model_summary_list),
    exists("model_summary_long", growth_model_summary_list),
    is.logical(use_knit_print)
  )

  # Extract model_summary_long from growth_model_summary_list object
  data_frame <- growth_model_summary_list[["model_summary_long"]]

  # Create flextable object
  flx_tbl <- flextable(data_frame) %>%
    flextable::hline_top(
      border = flextable::fp_border_default(width = 0),
      part = "header"
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(
      align = "center", part = "body",
      j = c(2:ncol(data_frame))
    ) %>%
    flextable::align(align = "left", part = "body", j = 1) %>%
    flextable::fontsize(size = 14, part = "header") %>%
    flextable::font(fontname = "Albany AMT", part = "body") %>%
    flextable::font(fontname = "Albany AMT", part = "header") %>%
    bold(part = "header", bold = TRUE) %>%
    flextable::fontsize(size = 12, part = "body") %>%
    flextable::set_table_properties(layout = "autofit")
  # If use_knitr is TRUE, knit the flx_tbl using knit_print, otherwise return the flx_tbl object
  if (use_knit_print == TRUE) {
    knitr::knit_print(flx_tbl)
  } else {
    return(flx_tbl)
  }
}
