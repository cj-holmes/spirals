#' Spiral plot
#'
#' This code is designed for artistic/aesthetic visualistion of numbers only. It should not be used for serious data visualisation for the following two reasons
#'  \itemize{
#'    \item The data may not be fully or correctly reproduced on the spiral
#'    \item Polar coordinates have major perceptual problems
#'   }
#'
#' The data is scaled to sit symmetrically above and below the spiral line
#'
#' @param x numeric vector to plot in a spiral
#' @param n number of 'rings' in spiral (default = 5)
#' @param r centre hole size as percentage of spiral thickness (default = 25)
#' @param m direction of spiral (-1 spiral in towards centre, 1 spiral away from centre (default = -1))
#' @param bg_fill background fill colour
#' @param line_col spiral line colour
#' @param line_width spiral line width
#' @param buffer spacing buffer (default = 0.9)
#' @param infill in-fill value for extra points needed to fill final spiral ring (default = NA)
#' @param ... further arguments passed to \code{ggplot2::geom_line()}
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' spiral(rnorm(5000), n=8)
spiral <- function(x, n = 5, r = 25, m = -1,
                   bg_fill = "white", line_col = "black",
                   line_width = 0.05, buffer = 0.9,
                   infill = NA, ...){

  if(!(m %in% c(1, -1))){stop("m must be 1 or -1")}

  n_data <- length(x)

  # Points per ring (the number of equally separated points that make up each ring of the spiral)
  # The one is added to ensure that the total number of points is more than the length of the data. When chunks
  # are computed (with the first y-value of a chunk is the same as the last y-value of the preceeding chunk), this should
  # ensure that all original data points are in the visualisation
  ppr <- ceiling((n_data/n)) + 1

  # Total points in visualisation
  t_points <- ppr * n

  # Difference between total points in n_rings FULL rings and actual data points
  delta <- t_points - n_data

  # Straight line offset (radius of whole spiral from origin of plot)
  single_ring_thickness <- abs(m) * (ppr-1)
  c <- (n * single_ring_thickness)

  # Centre hole offset
  ru <- (c/100) * r
  c <- c + ru

  # Max amplitude for signal to not bleed across the spiral rings
  scaled_data <- scales::rescale(x, to=c(-1,1)) * (single_ring_thickness/2) * buffer

  # Create dataframe
  df <-
    tibble::tibble(x = 1:t_points,
                   y = c(scaled_data, rep(infill, delta))) %>%
    dplyr::mutate(g = as.factor(rep(1:n, each = ppr)),
                  y2 = y + ((m * x) + c))

  # Compute x and y chunks so that in polar coordinates the same x points are repeated
  # and the last point in y of chunk 1 is the first point in y of chunk 2 (and so on...)
  # This ensures the individual spiral rings line up perfectly
  # Create the x-chunks
  x_chunks <- rep(0:(ppr-1), times = n)
  df$x_chunks <- x_chunks

  # Create the y-chunks
  step <- ppr - 1
  y_chunk_indices <- purrr::map(1:n, ~(1:ppr) + (step * (.x-1)))
  y_chunks <- purrr::map(y_chunk_indices, ~df$y2[.x])
  df$y_chunks <- unlist(y_chunks)

  ggplot2::ggplot(df)+
    ggplot2::geom_line(ggplot2::aes(x_chunks, y_chunks, group = g), col=line_col, size=line_width, ...)+
    ggplot2::expand_limits(y=0)+
    ggplot2::coord_polar()+
    ggplot2::theme_void()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_fill, color=NA))

}




