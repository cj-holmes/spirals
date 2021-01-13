library(tidyverse)

t <- tuneR::readMP3('tests/deadbeat.mp3')

gm <- function(x, n) tapply(x, rep(seq_along(x), each = n, length.out=length(x)), mean)

# Create the 'reduced' trace (average every ~30 values)
t_reduced <- gm(t@left, 30)

spiral(t_reduced)
ggsave("tests/mp3-default.pdf", width = 12, height = 12, units = "in")


spiral(t_reduced,
       n=20,
       r=30,
       bg_fill = "cornsilk1",
       line_width = 0.05,
       mult_buffer = 1,
       line_col="steelblue1")

ggsave("tests/mp3-out.pdf", width = 12, height = 12, units = "in")
