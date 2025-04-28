# Load required libraries
library(dplyr)
library(readr)

# Read your datasets
df_u <- read_csv("output/full_evaluation_dataset_u.csv")
df_d <- read_csv("output/full_evaluation_dataset_d.csv")
df_f <- read_csv("output/full_evaluation_dataset_f.csv")

# Function to summarize variability for a given metric:
summarize_variability <- function(data, metric) {
  data %>%
    group_by(user_count, perm_count, alpha) %>%
    summarize(
      std_dev = sd(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(config_label = paste0(user_count, " users, ", perm_count, " perms, alpha = ", alpha)) %>%
    summarize(
      mean_std_dev = mean(std_dev, na.rm = TRUE),
      max_std_dev = max(std_dev, na.rm = TRUE),
      max_config = config_label[which.max(std_dev)],
      min_std_dev = min(std_dev, na.rm = TRUE),
      min_config = config_label[which.min(std_dev)],
      .groups = "drop"
    )
}

# Function to run and print results for all metrics and a given dataset:
print_dataset_summary <- function(data, dataset_label) {
  cat("\n==============================\n")
  cat(paste("📌 Variability Summary for:", dataset_label, "\n"))
  cat("==============================\n")
  
  cluster_summary <- summarize_variability(data, "clusters")
  modularity_summary <- summarize_variability(data, "modularity")
  jaccard_summary <- summarize_variability(data, "jaccard")
  
  cat("\n➡️ Cluster Count Variability:\n")
  print(cluster_summary)
  
  cat("\n➡️ Modularity Variability:\n")
  print(modularity_summary)
  
  cat("\n➡️ Jaccard Similarity Variability:\n")
  print(jaccard_summary)
}

# Run for all datasets
print_dataset_summary(df_u, "Dataset U")
print_dataset_summary(df_d, "Dataset D")
print_dataset_summary(df_f, "Dataset F (Firewall1)")
