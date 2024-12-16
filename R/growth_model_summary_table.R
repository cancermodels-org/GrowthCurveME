#' Create a printable table of the summarized growth model result reporting
#'
#' @description
#' This function creates a flextable object that can be used
#' for documentation or Rmarkdown reports from the list object created
#' by \code{\link{growth_curve_model_fit}}.
#' The 'model_summary_long' data frame from the list object is used to
#' generate the table.
#'
#' @inheritParams growth_model_residual_plots
#' @param font_name A character string specifying the name of the font to use
#' when rendering the table. Defaults to "Albany AMT".
#' See \code{\link[flextable]{font}}.
#' @param font_size_header A numeric value specifying the size of the font
#' for the header of the table. Defaults to 14.
#' See \code{\link[flextable]{fontsize}}.
#' @param font_size_body A numeric value specifying the size of the font
#' for the body of the table. Defaults to 12.
#' See \code{\link[flextable]{fontsize}}.
#' @param use_knit_print A logical value to specify whether the flextable
#' should be printed using \code{\link[knitr]{knit_print}} function
#' instead of the flextable object being returned. Defaults to FALSE.
#'
#' @return A flextable object of the 'model_summary_long' data frame.
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @importFrom flextable align autofit bold hline_top flextable
#' fontsize fp_border_default
#' @importFrom knitr knit_print
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model_summary <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' verbose = FALSE)
#' # Create flextable object of the growth model results
#' exp_model_table <- growth_model_summary_table(
#' growth_model_summary_list = exp_mixed_model_summary)
#' # Print the table in the view pane
#' exp_model_table
growth_model_summary_table <- function(growth_model_summary_list,
                                       font_name = "Albany AMT",
                                       font_size_header = 14,
                                       font_size_body = 12,
                                       use_knit_print = FALSE) {
  # Check function inputs
  stopifnot(
    is.list(growth_model_summary_list),
    exists("model_summary_long", growth_model_summary_list),
    is.character(font_name),
    is.numeric(font_size_header),
    is.numeric(font_size_body),
    is.logical(use_knit_print)
  )

  # Extract model_summary_long from growth_model_summary_list object
  data_frame <- growth_model_summary_list[["model_summary_long"]]

  # Create flextable object
  flx_tbl <- flextable::flextable(data_frame) %>%
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
    flextable::fontsize(size = font_size_header, part = "header") %>%
    flextable::fontsize(size = font_size_body, part = "body") %>%
    flextable::font(fontname = font_name, part = "body") %>%
    flextable::font(fontname = font_name, part = "header") %>%
    flextable::bold(part = "header", bold = TRUE) %>%
    flextable::autofit()

  # If use_knitr is TRUE, knit the flx_tbl using knit_print,
  # otherwise return the flx_tbl object
  if (use_knit_print == TRUE) {
    knitr::knit_print(flx_tbl)
  } else {
    return(flx_tbl)
  }
}
