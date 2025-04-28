# Load necessary libraries
library(ggplot2)
library(viridis)
library(dplyr)

# Define your datasets and file paths
datasets <- list(
  Dataset_F = "output/full_evaluation_dataset_f.csv",
  Dataset_D = "output/full_evaluation_dataset_d.csv",
  Dataset_U = "output/full_evaluation_dataset_u.csv"
)

# Create output directory if it doesn't exist
if (!dir.exists("output2")) {
  dir.create("output2")
}

# Academic-friendly color palette
color_palette <- scale_fill_viridis_d(option = "viridis")

# Function to generate boxplots for a given metric and dataset name
plot_metric <- function(data, metric, y_label, dataset_name) {
  # Clean dataset names for human-readable titles
  dataset_title <- case_when(
    dataset_name == "Dataset_F" ~ "Firewall Dataset",
    dataset_name == "Dataset_D" ~ "Domino Dataset",
    dataset_name == "Dataset_U" ~ "University Dataset",
    TRUE ~ dataset_name
  )
  
  # Convert factors for correct ordering
  data$alpha <- as.factor(data$alpha)
  data$user_count <- as.factor(data$user_count)
  data$perm_count <- as.factor(data$perm_count)
  
  # Plot
  p <- ggplot(data, aes(x = alpha, y = .data[[metric]], fill = user_count)) +
    geom_boxplot(outlier.shape = 21, outlier.size = 1, outlier.stroke = 0.5) +
    facet_grid(user_count ~ perm_count, labeller = label_both) +
    color_palette +
    labs(
      title = paste0(y_label, " Consistency Across Repetitions\n(", dataset_title, ")"),
      x = "Target Jaccard Similarity (α)",
      y = y_label,
      fill = "User Count"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  
  return(p)
}

# Loop through each dataset and generate the plots
for (dataset_name in names(datasets)) {
  df <- read.csv(datasets[[dataset_name]])
  
  # Generate plots for each metric
  p_clusters <- plot_metric(df, "clusters", "Cluster Count", dataset_name)
  p_modularity <- plot_metric(df, "modularity", "Modularity", dataset_name)
  p_jaccard <- plot_metric(df, "jaccard", "Average Jaccard Similarity", dataset_name)
  
  # Save the plots as high-res PNGs (600 dpi)
  ggsave(paste0("output2/", dataset_name, "_clusters_consistency.png"), plot = p_clusters, width = 10, height = 8, dpi = 600)
  ggsave(paste0("output2/", dataset_name, "_modularity_consistency.png"), plot = p_modularity, width = 10, height = 8, dpi = 600)
  ggsave(paste0("output2/", dataset_name, "_jaccard_consistency.png"), plot = p_jaccard, width = 10, height = 8, dpi = 600)
}
