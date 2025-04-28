# === STEP 0: Libraries ===
library(Matrix)
library(proxy)
library(igraph)
library(dplyr)
library(ggplot2)

# === STEP 1: Define Helper Functions ===
calculate_jaccard <- function(mat) {
  sim <- proxy::simil(as.matrix(mat), method = "Jaccard")
  mean(sim[lower.tri(sim)], na.rm = TRUE)
}

calculate_louvain <- function(mat) {
  sim <- proxy::simil(as.matrix(mat), method = "Jaccard")
  sim[is.na(sim)] <- 0
  g <- graph_from_adjacency_matrix(as.matrix(sim), mode = "undirected", weighted = TRUE, diag = FALSE)
  if (gsize(g) == 0) return(list(modularity = NA, clusters = NA))
  lc <- cluster_louvain(g)
  return(list(modularity = modularity(lc), clusters = length(unique(membership(lc)))))
}

# === STEP 2: Define Dataset Paths ===
datasets <- list(
  "Dataset_F" = "F-UPA.txt",  
  "Dataset_D" = "D-UPA.txt",
  "Dataset_SC" = "SC-UPA.txt",
  "Dataset_U" = "U-UPA.txt"
)

results <- list()

# === STEP 3: Analyze Each Dataset ===
for (name in names(datasets)) {
  cat("Processing:", name, "\n")
  U <- as.matrix(read.table(datasets[[name]]))
  U <- apply(U, 2, as.numeric)  # Ensure numeric
  
  n <- nrow(U)
  m <- ncol(U)
  density <- sum(U) / (n * m)
  jaccard <- calculate_jaccard(U)
  louvain <- calculate_louvain(U)
  
  results[[name]] <- data.frame(
    Dataset = name,
    Users = n,
    Permissions = m,
    Density = round(density, 4),
    Avg_Jaccard = round(jaccard, 4),
    Cluster_Count = louvain$clusters,
    Modularity = round(louvain$modularity, 4)
  )
}

# === STEP 4: Combine Results and Save ===
df_summary <- do.call(rbind, results)
print(df_summary)
write.csv(df_summary, "output/dataset_comparison_summary.csv", row.names = FALSE)

# === STEP 5: Optional Visualization ===
ggplot(df_summary, aes(x = Dataset, y = Avg_Jaccard, fill = Dataset)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Jaccard Similarity per Dataset", y = "Avg Jaccard", x = "Dataset") +
  theme_minimal(base_size = 13) +
  ggsave("output/jaccard_comparison_plot.png", width = 8, height = 5, dpi = 300)

cat("\n✅ Comparison complete! Summary and plot saved in 'output/' folder.\n")
