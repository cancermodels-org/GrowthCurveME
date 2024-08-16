#' Create bootstrapped confidence interval plot from fixed-effects of growth model
#'
#' @description
#' The function leverages functions from the 'nlraa' package to create bootstrapped confidence intervals of the fixed-effects of a growth model fit from the function \code{\link{growth_curve_model_fit}} and summarized by \code{\link{summarize_growth_model}}.
#'
#'
#' @inheritParams summarize_growth_model
#' @inheritParams growth_model_residual_plots
#' @param annotate_value A character string specifying whether the doubling time estimate with 95%CI or rate constant estimate with 95%CI derived from the growth_model_object should be displayed on the plot.
#' Options include "NA" for no annotation, "double_time" for the doubling time with 95%CI, or "rate_constant" for the rate constant with 95%CI.
#' Defaults to "double_time".
#' Please note that when the growth model is a mixed-effects model, the 95%CI annotation accounts for fixed and random effects, where as the 95%CI plotted on the graph is generated from bootstrapping the fixed effects of the model only.
#' @param return A character string, options include "plot" for returning the plot generated from the function or "estimates" to return a data frame with the predicted bootstrap confidence values for each observation. Defaults to "plot".
#' @param growth_metric_name A character string for specifying the name of the growth metric (y-axis title) to be displayed on the plot. Defaults to "growth_metric".
#' @param time_name A character string for specifying the name of the time variable (x-axis title) to be displayed on the plot. Defaults to "time".
#' @param plot_title A character string for specifying the title to be displayed over the plot. Defaults to "Regression Model with 95% Confidence Intervals".
#' @param boot_n_sim A numeric value specifying the number of simulations for \code{\link[nlraa]{predict_nlme}} or \code{\link[nlraa]{predict_nls}} to perform to create the confidence intervals. Defaults to 1000.
#' @param x_limits A numeric vector of length two providing limits for the x-axis. Use NA to refer to the existing minimum or maximum. Defaults to c(NA, NA). See \code{\link[ggplot2]{scale_x_continuous}}.
#' @param n_x_axis_breaks An integer specifying the number of major breaks for the x-axis. Defaults to NULL. See \code{\link[ggplot2]{scale_x_continuous}}.
#' @param y_limits A numeric vector of length two providing limits for the y-axis. Use NA to refer to the existing minimum or maximum. Defaults to c(NA, NA). See \code{\link[ggplot2]{scale_y_continuous}}.
#' @param n_y_axis_breaks An integer specifying the number of major breaks for the x-axis. Defaults to NULL. See \code{\link[ggplot2]{scale_y_continuous}}.
#' @param x_axis_text_size A numeric value specifying the size of the x-axis text. Defaults to 8. See \code{\link[ggplot2]{element_text}}.
#' @param y_axis_text_size A numeric value specifying the size of the y-axis text. Defaults to 12. See \code{\link[ggplot2]{element_text}}.
#' @param x_axis_title_size A numeric value specifying the size of the x-axis title. Defaults to 14. See \code{\link[ggplot2]{element_text}}.
#' @param y_axis_title_size A numeric value specifying the size of the y-axis title. Defaults to 14. See \code{\link[ggplot2]{element_text}}.
#' @param plot_title_size A numeric value specifying the size of the plot title. Defaults to 20. See \code{\link[ggplot2]{element_text}}.
#' @param annotate_value_text_size A numeric value specifying the size of the annotation text. Defaults to 6. See \code{\link[ggplot2]{geom_text}}.
#' @param ... Additional arguments that can be passed to \code{\link[nlraa]{predict_nlme}} or \code{\link[nlraa]{predict_nls}}
#'
#' @return When return = "plot", returns either a plot of the growth model's fixed-effects predicted values as a line and 95% bootstrapped confidence intervals as a shaded region.
#' When return = "estimates", return a data frame with fixed-effects predicted values and 95% bootstrapped confidence intervals for each observation.
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}}
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate
#' @importFrom nlraa predict_nlme predict_nls
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(data_frame = exp_mixed_data, function_type = "exponential")
#' # Summarize the data by creating a summary list object
#' exp_mixed_model_summary <- summarize_growth_model(data_frame = exp_mixed_data, growth_model_object = exp_mixed_model, model_type = "mixed", function_type = "exponential")
#' # Create a plot of fixed-effects bootstrapped 95%CI with the doubling time estimates from the exp_mixed_model annotated on the plot
#' exp_ci_plot <- growth_model_boot_ci_curve(growth_model_object = exp_mixed_model,
#' growth_model_summary_list = exp_mixed_model_summary,
#' model_type = "mixed",
#' annotate_value = "double_time",
#' return = "plot")
#' print(exp_ci_plot)
growth_model_boot_ci_curve <- function(growth_model_object,
                                       growth_model_summary_list,
                                       model_type = "mixed",
                                       annotate_value = "double_time",
                                       return = "plot",
                                       growth_metric_name = "growth_metric",
                                       time_name = "time",
                                       plot_title = "Regression Model with 95% Confidence Intervals",
                                       boot_n_sim = 1000,
                                       x_limits = c(NA, NA),
                                       n_x_axis_breaks = NULL,
                                       y_limits = c(NA, NA),
                                       n_y_axis_breaks = NULL,
                                       x_axis_text_size = 8,
                                       y_axis_text_size = 12,
                                       x_axis_title_size = 14,
                                       y_axis_title_size = 14,
                                       plot_title_size = 20,
                                       annotate_value_text_size = 6,
                                       ...) {
  # Check initial function inputs
  stopifnot(
    inherits(growth_model_object, c("lm", "nls", "lme", "nlme")),
    is.list(growth_model_summary_list),
    exists("model_summary_long", growth_model_summary_list),
    exists("model_residual_data", growth_model_summary_list),
    model_type %in% c("mixed", "least-squares"),
    annotate_value %in% c("NA", "double_time", "rate_constant"),
    return %in% c("plot", "estimates"),
    is.character(growth_metric_name),
    is.character(time_name),
    is.character(plot_title),
    is.numeric(boot_n_sim),
    boot_n_sim > 0,
    is.vector(x_limits),
    length(x_limits) == 2,
    (is.null(n_x_axis_breaks) | is.numeric(n_x_axis_breaks)),
    is.vector(y_limits),
    length(y_limits) == 2,
    (is.null(n_y_axis_breaks) | is.numeric(n_y_axis_breaks)),
    is.numeric(x_axis_text_size),
    is.numeric(y_axis_text_size),
    is.numeric(x_axis_title_size),
    is.numeric(y_axis_title_size),
    is.numeric(plot_title_size),
    is.numeric(annotate_value_text_size)
  )

  # Extract the model residual data from growth_model_summary_list
  data_frame <- growth_model_summary_list[["model_residual_data"]]

  # Calculate confidence intervals using predict_nlme from nlraa package
  if (model_type == "mixed") {
    confidence_values <- nlraa::predict_nlme(growth_model_object,
      newdata = data_frame,
      interval = "confidence",
      nsim = boot_n_sim,
      ...
    )
  } else {
    confidence_values <- nlraa::predict_nls(growth_model_object,
      newdata = data_frame,
      interval = "confidence",
      nsim = boot_n_sim,
      ...
    )
  }

  # Join predicted CI values with data_frame
  data_frame_boot_data <- data_frame %>%
    dplyr::mutate(
      estimate = confidence_values[, 1],
      est_error = confidence_values[, 2],
      ci_lower = confidence_values[, 3],
      ci_upper = confidence_values[, 4]
    )

  if (return == "plot") {
    # Create CI plot
    confidence_plot <- ggplot2::ggplot(
      data = data_frame_boot_data,
      ggplot2::aes(x = time)
    ) +
      ggplot2::geom_line(ggplot2::aes(y = estimate),
        linewidth = 1,
        color = "black"
      ) +
      ggplot2::geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
        alpha = 0.5,
        fill = "#90d743"
      ) +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        limits = x_limits,
        n.breaks = n_x_axis_breaks
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = y_limits,
        n.breaks = n_y_axis_breaks
      ) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::xlab(time_name) +
      ggplot2::ylab(growth_metric_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = plot_title_size, face = "bold"),
        axis.title.x = ggplot2::element_text(size = x_axis_title_size, color = "black", face = "bold"),
        axis.title.y = ggplot2::element_text(size = y_axis_title_size, colour = "black", face = "bold"),
        axis.text.x = ggplot2::element_text(size = x_axis_text_size, color = "black", face = "bold"),
        axis.text.y = ggplot2::element_text(size = y_axis_text_size, color = "black", face = "bold"),
        legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::coord_cartesian(clip = "off")

    # Add annotation if provided
    if (annotate_value == "double_time") {
      # Extract doubling time from model_summary_long dataset from list object
      plot_label_data <- growth_model_summary_list[["model_summary_long"]] %>%
        dplyr::filter(Variable == "Doubling time estimate [95% CI]")
      annotate_data <- data.frame(
        x_pos = -Inf, y_pos = Inf,
        text = paste(" Doubling Time:", plot_label_data[1, 2]),
        h_just = 0, v_just = 1
      )

      confidence_plot <- confidence_plot +
        ggplot2::geom_text(
          data = annotate_data,
          ggplot2::aes(
            x = x_pos, y = y_pos,
            hjust = h_just, vjust = v_just, label = text
          ),
          size = annotate_value_text_size
        )
    }
    if (annotate_value == "rate_constant") {
      plot_label_data <- growth_model_summary_list[["model_summary_long"]] %>%
        dplyr::filter(Variable == "Rate estimate [95% CI]")
      annotate_data <- data.frame(
        x_pos = -Inf, y_pos = Inf,
        text = paste(" Rate:", plot_label_data[1, 2]),
        h_just = 0, v_just = 1
      )
      confidence_plot <- confidence_plot +
        ggplot2::geom_text(
          data = annotate_data, aes(
            x = x_pos, y = y_pos,
            hjust = h_just, vjust = v_just, label = text
          ),
          size = annotate_value_text_size
        )
    }

    return(confidence_plot)
  } else {
    return(data_frame_boot_data)
  }
}
