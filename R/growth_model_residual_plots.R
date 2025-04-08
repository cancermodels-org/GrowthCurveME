#' Create residual diagnostic plots for growth model
#'
#' @description
#' This function provides a wrapper to ggplot2 for generating residual
#' diagnostic plots and summary statistics for a growth model summary list
#' object produced by \code{\link{growth_curve_model_fit}}.
#'
#'
#' @param growth_model_summary_list A list object created by the
#' \code{\link{growth_curve_model_fit}} function.
#' @param residual_type A character string specifying the type of residuals
#' to be displayed in the plot. Options include "population" for the
#' fixed-effects residuals for mixed-effects and least-squares models and
#' "cluster" for fixed and random-effects residuals for mixed-effects
#' regression models. Defaults to "cluster".
#' @param weighted A logical value, when TRUE displays weighted residuals for
#' mixed-effects models or standardized residuals for least-squares models,
#' when FALSE displays the raw residuals for mixed-effects and least-squares
#' models. Defaults to "TRUE".
#'
#' @return Returns a patchwork collage of ggplot2 model diagnostic plots with
#' the following plots displayed:
#' \itemize{
#'  \item Residual vs Fitted Values - a model diagnostic plot for assessing
#'  the distribution of the residuals vs the model fitted values,
#'  useful in detecting improper function specification, homogeneity of
#'  variance, and outlier detection.
#'  \item Q-Q Plot - a model diagnostic plot (quantile-quantile) plot for
#'  comparing the residuals vs their theoretical quantiles,
#'  useful in assessing normality assumptions and outlier detection.
#'  \item Residual Density Plot - a model diagnostic showing the distribution
#'  of the residuals (histogram) with a normal distribution curve
#'  overlaid based on the residuals mean and standard deviation, useful in
#'  assessing normality assumptions and skewness.
#'  \item Residual Summary Statistics - a list of descriptive statistics of
#'  the the residuals including: mean, median, minimum, maximum,
#'  skewness, and kurtosis.
#' }
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr pull select
#' @importFrom patchwork plot_annotation plot_layout wrap_plots
#' @importFrom moments skewness kurtosis
#' @importFrom viridis scale_color_viridis
#' @importFrom stats density dnorm median sd
#' @importFrom graphics text
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' \donttest{
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data and produce summary list
#' exp_mixed_model_summary <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' verbose = FALSE)
#' # Check residuals and model assumptions
#' residual_diag_plot <- growth_model_residual_plots(
#'   growth_model_summary_list = exp_mixed_model_summary)
#' print(residual_diag_plot)
#' }
growth_model_residual_plots <- function(growth_model_summary_list,
                                        residual_type = "cluster",
                                        weighted = TRUE) {
  # Check initial inputs
  stopifnot(
    is.list(growth_model_summary_list),
    exists("model_residual_data", growth_model_summary_list),
    exists("model_summary_wide", growth_model_summary_list),
    residual_type %in% c("cluster", "population"),
    is.logical(weighted)
  )

  # Extract model type from model_summary_wide
  model_type <- growth_model_summary_list[["model_summary_wide"]] %>%
    dplyr::pull(!!rlang::sym("model_type")) %>%
    as.character()

  if (model_type == "least-squares" &
    residual_type != "population") {
    warn_message <- paste0("residual_type set to 'population' due to",
    " least-squares model specification")
    message(warn_message)
    residual_type <- "population"
  }

  # Extract model residual data from list object
  model_residual_data <- growth_model_summary_list[["model_residual_data"]]

  # Set variables depending on inputs
  if (model_type == "mixed-effects") {
    if (residual_type == "population") {
      if (weighted) {
        model_residual_data <- model_residual_data %>%
          dplyr::select(!!rlang::sym("cluster"),
            fitted_v = !!rlang::sym("pop_fit_value"),
            residual_var = !!rlang::sym("pop_wt_resid"),
            theoretical_var = !!rlang::sym("pop_wt_resid_quant")
          )
        residual_title <- "ME: Population-Level Weighted Residuals"
        res_axis <- "Weighted Residuals"
      } else {
        model_residual_data <- model_residual_data %>%
          dplyr::select(!!rlang::sym("cluster"),
            fitted_v = !!rlang::sym("pop_fit_value"),
            residual_var = !!rlang::sym("pop_resid"),
            theoretical_var = !!rlang::sym("pop_resid_quant")
          )
        residual_title <- "ME: Population-Level Residuals"
        res_axis <- "Residuals"
      }
    } else {
      if (weighted) {
        model_residual_data <- model_residual_data %>%
          dplyr::select(!!rlang::sym("cluster"),
            fitted_v = !!rlang::sym("ind_fit_value"),
            residual_var = !!rlang::sym("ind_wt_resid"),
            theoretical_var = !!rlang::sym("ind_wt_resid_quant")
          )
        residual_title <- "ME: Cluster-Level Weighted Residuals"
        res_axis <- "Weighted Residuals"
      } else {
        model_residual_data <- model_residual_data %>%
          dplyr::select(!!rlang::sym("cluster"),
            fitted_v = !!rlang::sym("ind_fit_value"),
            residual_var = !!rlang::sym("ind_resid"),
            theoretical_var = !!rlang::sym("ind_resid_quant")
          )
        residual_title <- "ME: Cluster-Level Residuals"
        res_axis <- "Residuals"
      }
    }
  } else {
    if (weighted) {
      model_residual_data <- model_residual_data %>%
        dplyr::select(!!rlang::sym("cluster"),
          fitted_v = !!rlang::sym("pop_fit_value"),
          residual_var = !!rlang::sym("pop_stand_resid"),
          theoretical_var = !!rlang::sym("pop_stand_resid_quant")
        )
      residual_title <- "LS: Standardized Residuals"
      res_axis <- "Standardized Residuals"
    } else {
      model_residual_data <- model_residual_data %>%
        dplyr::select(!!rlang::sym("cluster"),
          fitted_v = !!rlang::sym("pop_fit_value"),
          residual_var = !!rlang::sym("pop_resid"),
          theoretical_var = !!rlang::sym("pop_resid_quant")
        )
      residual_title <- "LS: Residuals"
      res_axis <- "Residuals"
    }
  }

  # Create vector of residuals
  residual_vector <- model_residual_data %>%
    dplyr::pull(!!rlang::sym("residual_var"))

  # QQ-Plot
  if (model_type == "mixed-effects") {
    qq_plot <- ggplot2::ggplot(
      model_residual_data,
      ggplot2::aes(
        x = !!rlang::sym("theoretical_var"),
        y = !!rlang::sym("residual_var"),
        color = !!rlang::sym("cluster")
      )
    ) +
      ggplot2::geom_abline(slope = 1, intercept = 0, lwd = 0.75) +
      ggplot2::geom_point(
        alpha = 0.75, size = 3,
        show.legend = FALSE
      )
  } else {
    qq_plot <- ggplot2::ggplot(
      model_residual_data,
      ggplot2::aes(
        x = !!rlang::sym("theoretical_var"),
        y = !!rlang::sym("residual_var"),
        color = !!rlang::sym("cluster")
      )
    ) +
      ggplot2::geom_abline(slope = 1, intercept = 0, lwd = 0.75) +
      ggplot2::geom_point(
        # color = "#a0da39",
        alpha = 0.75, size = 3,
        show.legend = FALSE
      )
  }
  qq_plot <- qq_plot +
    ggplot2::theme_classic() +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::ggtitle("Q-Q Plot") +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab(res_axis) +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle =
        ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x =
        ggplot2::element_text(size = 14, color = "black", face = "bold"),
      axis.title.y =
        ggplot2::element_text(size = 14, colour = "black", face = "bold"),
      axis.text.x =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      axis.text.y =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      legend.title =
        ggplot2::element_text(hjust = 0.5, face = "bold")
    ) + ggplot2::labs(color = "cluster")

  # Residual density plot
  residual_density <- ggplot2::ggplot(
    model_residual_data,
    ggplot2::aes(x = !!rlang::sym("residual_var"))
  ) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 20,
      fill = "#a0da39",
      color = "black"
    ) +
    ggplot2::stat_function(
      fun = dnorm, args =
        list(
          mean = mean(residual_vector,
            na.rm = TRUE
          ),
          sd = sd(residual_vector,
            na.rm = TRUE
          )
        ),
      lwd = 1.5, color = "#46327e"
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle("Residual Density Plot") +
    ggplot2::xlab(res_axis) +
    ggplot2::ylab("Density") +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle =
        ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x =
        ggplot2::element_text(size = 14, color = "black", face = "bold"),
      axis.title.y =
        ggplot2::element_text(size = 14, colour = "black", face = "bold"),
      axis.text.x =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      axis.text.y =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      legend.title =
        ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Residual vs fitted plot
  if (model_type == "mixed-effects") {
    residual_v_fit <- ggplot2::ggplot(
      model_residual_data,
      ggplot2::aes(
        x = !!rlang::sym("fitted_v"),
        y = !!rlang::sym("residual_var"),
        color = !!rlang::sym("cluster")
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, lwd = 0.75,
                          linetype = "dashed", color = "grey50") +
      ggplot2::geom_point(
        alpha = 0.75, size = 3,
        show.legend = FALSE
      )
  } else {
    residual_v_fit <- ggplot2::ggplot(
      model_residual_data,
      ggplot2::aes(
        x = !!rlang::sym("fitted_v"),
        y = !!rlang::sym("residual_var"),
        color = !!rlang::sym("cluster")
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, lwd = 0.75,
                          linetype = "dashed", color = "grey50") +
      ggplot2::geom_point(
        # color = "#a0da39",
        alpha = 0.75, size = 3,
        show.legend = FALSE
      )
  }

  residual_v_fit <- residual_v_fit +
    ggplot2::theme_classic() +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggplot2::ggtitle("Residuals vs Fitted Values") +
    ggplot2::xlab("Fitted Values") +
    ggplot2::ylab(res_axis) +
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle =
        ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x =
        ggplot2::element_text(size = 14, color = "black", face = "bold"),
      axis.title.y =
        ggplot2::element_text(size = 14, colour = "black", face = "bold"),
      axis.text.x =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      axis.text.y =
        ggplot2::element_text(size = 12, color = "black", face = "bold"),
      legend.title =
        ggplot2::element_text(hjust = 0.5, face = "bold")
    ) + ggplot2::labs(color = "cluster")

  # Residual distribution table
  residual_table <- ggplot(
    data = data.frame(x = c(0, 1), y = c(0, 1)),
    ggplot2::aes(x = !!rlang::sym("x"), y = !!rlang::sym("y"))
  ) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    ggplot2::theme_classic() +
    ggplot2::ggtitle("Residual Statistics") +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.90,
                      size = 5, fontface = "bold", label = "Mean:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.90, size = 5,
      label = round(mean(residual_vector, na.rm = TRUE), 2)
    ) +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.75,
                      size = 5, fontface = "bold", label = "Median:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.75, size = 5,
      label = round(median(residual_vector, na.rm = TRUE), 2)
    ) +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.60,
                      size = 5, fontface = "bold", label = "Minimum:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.60, size = 5,
      label = round(min(residual_vector, na.rm = TRUE), 2)
    ) +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.45,
                      size = 5, fontface = "bold", label = "Maximum:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.45, size = 5,
      label = round(max(residual_vector, na.rm = TRUE), 2)
    ) +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.30,
                      size = 5, fontface = "bold", label = "Skewness:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.30, size = 5,
      label = round(moments::skewness(residual_vector), 2)
    ) +
    ggplot2::annotate(geom = "text", x = 0.35, y = 0.15,
                      size = 5, fontface = "bold", label = "Kurtosis:") +
    ggplot2::annotate(
      geom = "text", x = 0.65, y = 0.15, size = 5,
      label = round(moments::kurtosis(residual_vector, 2))
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5,
                                         size = 16, face = "bold"),
      line = ggplot2::element_blank(),
      text = ggplot2::element_blank(),
      title = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # Combine plots into one figure
  figure <- patchwork::wrap_plots(residual_v_fit, qq_plot,
    residual_density, residual_table,
    nrow = 2
  ) +
    patchwork::plot_layout(
      widths = c(1, 1),
      heights = c(0.5, 0.5)
    ) +
    patchwork::plot_annotation(
      title = residual_title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = 24,
          color = "black",
          face = "bold"
        )
      )
    )
  return(figure)
}
