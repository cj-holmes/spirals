library(tidyverse)

x <- runif(1479, min = -10, 1)

n <- 2
r <- 100
m = -1
lwd = 0.01
mult_buffer = 0.8
infill = 0


n_data <- length(x)

# Points per ring (the number of equally separated points that make up each ring of the spiral)
ppr <- ceiling((n_data/n)) + 1

# Total points in visualisation
t_points <- ppr * n

# Difference between total points in n_rings FULL rings and actual data points
delta <- t_points - n_data


# Straight line offset (radius of whole spiral from origin of plot)
single_ring_thickness <- abs(m) * (ppr-1)
c <- (n * single_ring_thickness)

ru <- (c/100) * r
c <- c + ru

# Max amplitude for signal to not bleed across the spiral rings (with 1% buffer)
dy <- (single_ring_thickness/2) * mult_buffer
scaled_data <- scales::rescale(x, to=c(-1,1)) * dy


# Create dataframe
df <-
  tibble::tibble(x = 1:t_points,
                 y = c(scaled_data, rep(infill, delta))) %>%
  dplyr::mutate(g = as.factor(rep(1:n, each = ppr)),
                y2 = y + ((m * x) + c),
                baseline = 0 + ((m * x) + c))

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

baseline_chunk_indices <- purrr::map(1:n, ~(1:ppr) + (step * (.x-1)))
baseline_chunks <- purrr::map(baseline_chunk_indices, ~df$baseline[.x])
df$baseline_chunks <- unlist(baseline_chunks)

patchwork::wrap_plots(

  ggplot2::ggplot(df)+
    ggplot2::geom_hline(aes(yintercept = c, col="start (c)"), size=1)+
    ggplot2::geom_hline(aes(yintercept = ru, col= "end (r)"), size=1)+
    ggplot2::geom_line(ggplot2::aes(x_chunks, y_chunks, group = g, col=g),
                       linejoin="bevel", size=lwd)+
    ggplot2::geom_line(aes(x_chunks, baseline_chunks, group=g))+
    ggplot2::expand_limits(y=0)+
    ggplot2::labs(x="x", y="y"),

  ggplot2::ggplot(df)+
    ggplot2::geom_hline(aes(yintercept = c, col="start (c)"), size=1)+
    ggplot2::geom_hline(aes(yintercept = ru, col = "end (r)"), size=1)+
    ggplot2::geom_line(aes(x_chunks, baseline_chunks, group=g))+
    ggplot2::expand_limits(y=0)+
    ggplot2::coord_polar()+
    ggplot2::labs(x="x", y="y"),

  ggplot2::ggplot(df)+
    ggplot2::geom_hline(aes(yintercept = c, col="start (c)"), size=1)+
    ggplot2::geom_hline(aes(yintercept = ru, col = "end (r)"), size=1)+
    ggplot2::geom_line(ggplot2::aes(x_chunks, y_chunks, group = g, col=g),
                       linejoin="bevel", size=lwd)+
    ggplot2::geom_line(aes(x_chunks, baseline_chunks, group=g))+
    ggplot2::expand_limits(y=0)+
    ggplot2::coord_polar()+
    ggplot2::labs(x="x", y="y"),

  ggplot2::ggplot(df)+
    ggplot2::geom_line(ggplot2::aes(x_chunks, y_chunks, group = g, col=g),
                       linejoin="bevel", size=lwd)+
    ggplot2::expand_limits(y=0)+
    ggplot2::coord_polar()+
    ggplot2::labs(x="x", y="y")+
    ggplot2::theme_void(),

  guides = "collect"
)
