# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Dataset paths
datasets <- list(
  F = "output/full_evaluation_dataset_f.csv",
  D = "output/full_evaluation_dataset_d.csv",
  U = "output/full_evaluation_dataset_u.csv"
)

# Function to generate median heatmaps with white-to-blue color scale
generate_median_heatmap <- function(data, metric, dataset_name) {
  summary_data <- data %>%
    group_by(user_count, perm_count, alpha) %>%
    summarize(median_value = median(.data[[metric]], na.rm = TRUE), .groups = "drop")
  
  # Plot
  p <- ggplot(summary_data, aes(x = factor(perm_count), y = factor(user_count), fill = median_value)) +
    geom_tile(color = "white") +
    facet_wrap(~ alpha, ncol = 3, labeller = label_bquote(alpha: ~ .(alpha))) +
    scale_fill_gradient(
      name = "Median",
      low = "white", high = "darkblue"
    ) +
    labs(
      title = paste0("Median ", toupper(substring(metric, 1, 1)), substring(metric, 2), " (", 
                     ifelse(dataset_name == "F", "Firewall Dataset",
                            ifelse(dataset_name == "D", "Domino Dataset", "University Dataset")), ")"),
      x = "Permission Count",
      y = "User Count"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      strip.text = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Save the plot
  ggsave(
    filename = paste0("figures/Dataset_", dataset_name, "_median_", metric, "_heatmap.png"),
    plot = p,
    width = 10, height = 4
  )
}

# Metrics to plot
metrics <- c("clusters", "modularity", "jaccard")

# Generate heatmaps for each dataset and metric
for (dataset_name in names(datasets)) {
  df <- read_csv(datasets[[dataset_name]])
  for (metric in metrics) {
    generate_median_heatmap(df, metric, dataset_name)
  }
}
