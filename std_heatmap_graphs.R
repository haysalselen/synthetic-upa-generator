# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Ensure output directory exists
if (!dir.exists("output2")) {
  dir.create("output2")
}

# Dataset paths
datasets <- list(
  Dataset_F = "output/full_evaluation_dataset_f.csv",
  Dataset_D = "output/full_evaluation_dataset_d.csv",
  Dataset_U = "output/full_evaluation_dataset_u.csv"
)

# Human-readable names
dataset_titles <- list(
  Dataset_F = "Firewall Dataset",
  Dataset_D = "Domino Dataset",
  Dataset_U = "University Dataset"
)

# Helper to compute standard deviation summaries per config
compute_std_summary <- function(df) {
  df %>%
    group_by(user_count, perm_count, alpha) %>%
    summarise(
      std_clusters = sd(clusters),
      std_modularity = sd(modularity),
      std_jaccard = sd(jaccard),
      .groups = "drop"
    ) %>%
    mutate(
      user_count = as.factor(user_count),
      perm_count = as.factor(perm_count),
      alpha = as.factor(alpha)
    )
}

# Heatmap plot function with consistent coloring (white to dark blue)
plot_heatmap_facet <- function(data, metric, metric_label, dataset_name, file_suffix) {
  p <- ggplot(data, aes(x = perm_count, y = user_count, fill = .data[[metric]])) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      name = "Std. Dev",
      low = "white",
      high = "#081d58"  # Dark academic blue, can adjust if needed
    ) +
    facet_wrap(~ alpha, nrow = 1, labeller = label_both) +
    labs(
      title = paste0(metric_label, " Variability (", dataset_name, ")"),
      x = "Permission Count",
      y = "User Count"
    ) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid = element_blank()
    )
  
  ggsave(
    filename = paste0("output2/", file_suffix, "_", metric, "_heatmap.png"),
    plot = p,
    width = 10,
    height = 4,
    dpi = 600
  )
}

# Run for each dataset
for (dataset_key in names(datasets)) {
  df <- read_csv(datasets[[dataset_key]])
  title <- dataset_titles[[dataset_key]]
  std_data <- compute_std_summary(df)
  
  plot_heatmap_facet(std_data, "std_clusters", "Cluster Count", title, dataset_key)
  plot_heatmap_facet(std_data, "std_modularity", "Modularity", title, dataset_key)
  plot_heatmap_facet(std_data, "std_jaccard", "Average Jaccard Similarity", title, dataset_key)
}

