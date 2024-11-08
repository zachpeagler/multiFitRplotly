#' Multiple PDF Plot For Continuous Variables
#' Using Plotly
#'
#' This function returns a plotly output showing the PDFs for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param x The variable to for which to plot PDFs
#' @param seq_length The number of points over which to fit x
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length(nrow) equal to seq_length +1.
#' @export
multiPDF_plotly <- function (x, seq_length, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multiPDF_cont(x, seq_length, distributions)
  # create plot with real density
  p <- plotly::plot_ly(data, x= ~x_seq, y= ~dens, type = 'scatter', mode = 'lines', name = 'Actual Density')
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~pdf_normal, name='Normal')
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~pdf_lognormal, name='Lognormal')
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~pdf_gamma, name='Gamma')
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~pdf_exponential, name='Exponential')
  }
  p <- p %>% plotly::layout(legend = list(orientation = "h",
                                          xanchor = "center",
                                          x = 0.5,
                                          yanchor = "top",
                                          y = -0.2,
                                          title = list(text = "Distribution")))
  return(p)
}

#' Multiple CDF Plot For Continuous Variables
#' Using Plotly
#'
#' This function returns a plotly output showing the CDFs for selected distributions
#' against a continuous, non-negative input variable. Possible distributions include "normal",
#' "lognormal", "gamma", "exponential", "cauchy", "t", "weibull", "logistic",
#' and "all".
#'
#' @param x The variable to for which to plot PDFs
#' @param seq_length The length over which to fit x
#' @param distributions The distributions to fit x against
#' @returns A dataframe with x, the real density, and the pdf of the desired
#'  distributions with length(nrow) equal to seq_length +1.
#' @export
multiCDF_plotly <- function (x, seq_length, distributions) {
  # check if "all" was passed to distributions
  if ("all" %in% distributions) {
    distributions <- c("normal",
                       "lognormal",
                       "gamma",
                       "exponential")
  }
  # calculate PDFs
  data <- multiCDF_cont(x, seq_length, distributions)
  # create plot with real density
  p <- plotly::plot_ly(data, x= ~x_seq, y= ~dens, type = 'scatter', mode = 'lines', name = 'Actual Distribution')
  # check for each type of distribution in the distributions, and add it if present
  if ("normal" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~cdf_normal, name='Normal')
  }
  if ("lognormal" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~cdf_lognormal, name='Lognormal')
  }
  if ("gamma" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~cdf_gamma, name='Gamma')
  }
  if ("exponential" %in% distributions == TRUE) {
    p <- p %>% plotly::add_trace(p, y=~cdf_exponential, name='Exponential')
  }
  p <- p %>% plotly::layout(legend = list(orientation = "h",
                                          xanchor = "center",
                                          x = 0.5,
                                          yanchor = "top",
                                          y = -0.2,
                                          title = list(text = "Distribution")))
  return(p)
}
