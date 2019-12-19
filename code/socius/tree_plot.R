# This file produces the 3D tree visualization
# in the introduction to the Special Issue of
# Socius about the Fragile Families Challenge

# Code by Ian Lundberg (ilundberg at princeton dot edu)

library(plotly)
library(here)

# Set output directory
results.dir <- file.path(here(), "results", "socius")

# Build plot
plot_ly(z = rbind(c(3,3,3.4,3.4),
                  c(3,3,3.4,3.4),
                  c(3,3,3.7,3.7),
                  c(3,3,3.7,3.7)),
        x = c(0,.5,.5,1),
        y = c(18,23,23,35)) %>% 
  add_surface() %>%
  layout(scene = list(xaxis=list(title="Mother completed college",
                                 ticktext = c("No","Yes"),
                                 tickvals = c(.25,.75)),
                      yaxis=list(title="Mother's age"),
                      zaxis=list(title="Predicted child GPA",
                                 range = c(3,4)))) ->
  p5

# Save plot manually as 5b_tree_figure_3d.pdf