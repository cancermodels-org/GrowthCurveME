#' Generate growth vs time plots
#'
#' @description
#' 'growth_vs_time_plot()' is a function that can be used to generate different plots from a list object created by the \code{\link{summarize_growth_model}} function.
#' Please refer to the documentation for the 'plot_type' parameter for the different plot options.
#'
#'
#' @inheritParams growth_model_boot_ci_curve
#' @param cluster_name A character string for specifying the name of the cluster variable (legend title) to be displayed on the plot. Defaults to "cluster".
#' @param plot_type A numeric value used to specify the plot type to graph. Values include 1, 2, or 3 with descriptions of each below (defaults to 2):
#'\itemize{
#'  \item 1 - A scatterplot of the growth_metric vs time data where each point is colored by cluster if applicable.
#'  \item 2 - A scatterplot of the growth_metric vs time data where each point is colored by cluster if applicable and the model predicted values are overlayed as line.
#'  When model_type = "mixed" the predicted values will be the fitted values accounting for both fixed and random effects (fitted_values_random).
#'  When model_type = "least-squares" the predicted values will be the fitted values accounting for fixed effects only (fitted_values_fixed).
#'  \item 3 - A scatterplot version of plot_type = 2 where each cluster is separated into their own plot forming a matrix of growth_metric vs time plots by cluster.
#'}
#' @param geom_point_size A numeric value specifying the size of the points on the graph. Defaults to 2. See \code{\link[ggplot2]{geom_point}}.
#' @param geom_line_width A numeric value specifying the width of the line (applicable only for plot_type = 2 or 3). Defaults to 0.5. See \code{\link[ggplot2]{geom_line}}.
#'
#' @return Returns a ggplot2 plot
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}}
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom viridis scale_color_viridis
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
#' # Create growth vs time plot of data with fitted values (plot_type = 2), adjust aesthetics and parameters as desired
#' exp_growth_plot <- growth_vs_time_plot(growth_model_summary_list = exp_mixed_model_summary,
#' model_type = "mixed",
#' plot_type = 2)
#' print(exp_growth_plot)
growth_vs_time_plot <- function(growth_model_summary_list,
                                model_type = "mixed",
                                growth_metric_name = "growth_metric",
                                time_name = "time",
                                cluster_name = "cluster",
                                plot_title = "Growth vs Time",
                                plot_type = 2,
                                x_limits = c(NA, NA),
                                n_x_axis_breaks = NULL,
                                y_limits = c(NA, NA),
                                n_y_axis_breaks = NULL,
                                x_axis_text_size = 8,
                                y_axis_text_size = 12,
                                x_axis_title_size = 14,
                                y_axis_title_size = 14,
                                plot_title_size = 20,
                                geom_point_size = 2,
                                geom_line_width = 0.5) {
  # Check initial function inputs
  stopifnot(
    is.list(growth_model_summary_list),
    exists("model_residual_data", growth_model_summary_list),
    model_type %in% c("mixed", "least-squares"),
    plot_type %in% c(1, 2, 3),
    is.character(growth_metric_name),
    is.character(time_name),
    is.character(plot_title),
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
    is.numeric(geom_point_size),
    is.numeric(geom_line_width)
  )

  # Extract the model residual data from growth_model_summary_list
  data_frame <- growth_model_summary_list[["model_residual_data"]]

  # Print plot with clusters all together, no model data
  if (plot_type == 1) {
    if (model_type == "mixed") {
      plot_1 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(
          x = time, y = growth_metric,
          color = cluster
        )
      ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7
        ) +
        viridis::scale_color_viridis(discrete = TRUE)
    } else {
      plot_1 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(x = time, y = growth_metric)
      ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7,
          color = "#a0da39"
        )
    }
    plot_1 <- plot_1 +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        expand = c(0, min(data_frame$time)),
        limits = x_limits,
        n.breaks = n_x_axis_breaks
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, min(data_frame$growth_metric)),
        limits = y_limits,
        n.breaks = n_y_axis_breaks
      ) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::xlab(time_name) +
      ggplot2::ylab(growth_metric_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = plot_title_size,
          face = "bold"
        ),
        axis.title.x = ggplot2::element_text(
          size = x_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = y_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = x_axis_text_size,
          color = "black", face = "bold"
        ),
        axis.text.y = ggplot2::element_text(
          size = y_axis_text_size,
          color = "black", face = "bold"
        ),
        legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::labs(color = cluster_name)

    return(plot_1)
  }

  # Create scatterplot with data points and line(s) by fitted model values
  if (plot_type == 2) {
    if (model_type == "mixed") {
      plot_2 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(
          x = time, y = growth_metric,
          color = cluster
        )
      ) +
        ggplot2::geom_line(
          aes(
            x = time, y = fitted_values_random,
            color = cluster
          ),
          lwd = geom_line_width
        ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7
        ) +
        viridis::scale_color_viridis(discrete = TRUE)
    } else {
      plot_2 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(x = time, y = growth_metric)
      ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7,
          color = "#a0da39"
        ) +
        ggplot2::geom_line(aes(x = time, y = fitted_values_fixed),
          lwd = geom_line_width,
          color = "black"
        )
    }
    plot_2 <- plot_2 +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        expand = c(0, min(data_frame$time)),
        limits = x_limits,
        n.breaks = n_x_axis_breaks
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, min(data_frame$growth_metric)),
        limits = y_limits,
        n.breaks = n_y_axis_breaks
      ) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::xlab(time_name) +
      ggplot2::ylab(growth_metric_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = plot_title_size,
          face = "bold"
        ),
        axis.title.x = ggplot2::element_text(
          size = x_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = y_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = x_axis_text_size,
          color = "black", face = "bold"
        ),
        axis.text.y = ggplot2::element_text(
          size = y_axis_text_size,
          color = "black", face = "bold"
        ),
        legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::labs(color = cluster_name)

    return(plot_2)
  }
  # Create scatterplot with data points and line(s) by fitted model values faceted by cluster
  if (plot_type == 3) {
    if (model_type == "mixed") {
      plot_3 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(
          x = time, y = growth_metric,
          color = cluster
        )
      ) +
        ggplot2::geom_line(
          aes(
            x = time, y = fitted_values_random,
            color = cluster
          ),
          lwd = geom_line_width,
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7,
          show.legend = FALSE
        ) +
        viridis::scale_color_viridis(discrete = TRUE)
    } else {
      plot_3 <- ggplot2::ggplot(
        data_frame,
        ggplot2::aes(x = time, y = growth_metric)
      ) +
        ggplot2::geom_point(
          size = geom_point_size,
          alpha = 0.7,
          color = "#a0da39",
          show.legend = FALSE
        ) +
        ggplot2::geom_line(aes(x = time, y = fitted_values_fixed),
          lwd = geom_line_width,
          color = "black",
          show.legend = FALSE
        )
    }
    plot_3 <- plot_3 +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        expand = c(0, min(data_frame$time)),
        limits = x_limits,
        n.breaks = n_x_axis_breaks
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, min(data_frame$growth_metric)),
        limits = y_limits,
        n.breaks = n_y_axis_breaks
      ) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::xlab(time_name) +
      ggplot2::ylab(growth_metric_name) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = plot_title_size,
          face = "bold"
        ),
        axis.title.x = ggplot2::element_text(
          size = x_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = y_axis_title_size,
          color = "black", face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = x_axis_text_size,
          color = "black", face = "bold"
        ),
        axis.text.y = ggplot2::element_text(
          size = y_axis_text_size,
          color = "black", face = "bold"
        ),
        legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::labs(color = cluster_name) +
      ggplot2::facet_wrap(~cluster)

    return(plot_3)
  }
}
