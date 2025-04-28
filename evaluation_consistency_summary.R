library(ggplot2)
library(dplyr)
library(readr)

# === STEP 1: Define Datasets and Paths ===
datasets <- list(
  Dataset_F = "output/full_evaluation_dataset_f.csv",
  Dataset_D = "output/full_evaluation_dataset_d.csv",
  Dataset_U = "output/full_evaluation_dataset_u.csv"
)

metrics <- c("jaccard", "modularity", "clusters")

# === STEP 2: Process Each Dataset ===
for (dataset_name in names(datasets)) {
  cat(paste0("\n📂 Processing ", dataset_name, "...\n"))
  
  # Load the dataset
  df <- read_csv(datasets[[dataset_name]])
  df$alpha <- as.factor(df$alpha)
  
  # === Generate Boxplots ===
  for (metric in metrics) {
    p <- ggplot(df, aes(x = alpha, y = .data[[metric]], fill = alpha)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      facet_grid(user_count ~ perm_count, labeller = label_both) +
      labs(title = paste(dataset_name, "- Distribution of", metric, "values"),
           x = "Desired Jaccard (α)", y = metric) +
      theme_minimal(base_size = 13)
    
    ggsave(
      filename = paste0("output/", dataset_name, "_boxplot_", metric, ".png"),
      plot = p, width = 10, height = 6, dpi = 300
    )
  }
  
  # === Create Summary Table ===
  summary_df <- df %>%
    group_by(user_count, perm_count, alpha) %>%
    summarise(
      jaccard_mean = mean(jaccard), jaccard_sd = sd(jaccard),
      modularity_mean = mean(modularity), modularity_sd = sd(modularity),
      clusters_mean = mean(clusters), clusters_sd = sd(clusters),
      .groups = 'drop'
    )
  
  write_csv(summary_df, paste0("output/", dataset_name, "_summary_results_table.csv"))
}

cat("\n✅ All datasets processed! Boxplots and summary tables saved to 'output/' folder.\n")
