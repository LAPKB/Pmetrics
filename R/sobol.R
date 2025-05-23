# Load the library
library(qrng)
library(plotly)
library(htmlwidgets)


# Generate 1024 points in 3D using Owen scrambling
n_points <- 1024
sobol_points_3d <- sobol(n = n_points, d = 3, randomize = "Owen", seed = 123)

# Convert to data frame for Plotly
sobol_df <- data.frame(
  x = sobol_points_3d[, 1] * 5,
  y = sobol_points_3d[, 2] * 5,
  z = sobol_points_3d[, 3] * 100
)
p <- plot_ly(
  data = sobol_df,
  x = ~x, y = ~y, z = ~z,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 2, color = 'royalblue')
) %>%
  layout(
    title = "3D Sobol Sequence with Owen Scrambling",
    scene = list(
      xaxis = list(title = "Ke"),
      yaxis = list(title = "Ka"),
      zaxis = list(title = "V")
    )
  )

p
# Save the plot as an HTML file
htmlwidgets::saveWidget(p, "sobol_3d_plot.html", selfcontained = TRUE)
