# 📦 Install and load required libraries
if (!require(proxy)) install.packages("proxy")
if (!require(igraph)) install.packages("igraph")
if (!require(ggplot2)) install.packages("ggplot2")

library(proxy)
library(igraph)
library(ggplot2)

# 📁 Create output directory
output_dir <- "toy_analysis_results"
dir.create(output_dir, showWarnings = FALSE)

# 🔧 Generate Toy UPA Matrices (including the new 3-clusters-overlap case)
generate_toy_upa <- function(type) {
  if (type == "collaborative") {
    upa <- matrix(1, nrow = 10, ncol = 10)
  } else if (type == "isolated") {
    upa <- diag(1, 10, 10)
  } else if (type == "2_clusters") {
    upa <- rbind(
      matrix(rep(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0), 5), nrow = 5, byrow = TRUE),
      matrix(rep(c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), 5), nrow = 5, byrow = TRUE)
    )
  } else if (type == "3_clusters") {
    upa <- rbind(
      matrix(rep(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0), 3), nrow = 3, byrow = TRUE),
      matrix(rep(c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0), 3), nrow = 3, byrow = TRUE),
      matrix(rep(c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1), 4), nrow = 4, byrow = TRUE)
    )
  } else if (type == "3_clusters_overlap") {
    upa <- rbind(
      matrix(rep(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 1), 3), nrow = 3, byrow = TRUE),  # P10 shared
      matrix(rep(c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1), 3), nrow = 3, byrow = TRUE),  # P10 shared
      matrix(rep(c(0, 0, 0, 0, 1, 1, 1, 1, 0, 1), 4), nrow = 4, byrow = TRUE)   # P10 shared
    )
  }
  colnames(upa) <- paste0("P", 1:ncol(upa))
  rownames(upa) <- paste0("U", 1:nrow(upa))
  return(upa)
}

# 🔍 Analyze a UPA matrix and return metrics
analyze_upa <- function(upa_matrix, label) {
  jaccard_mat <- 1 - as.matrix(dist(upa_matrix, method = "Jaccard"))
  jaccard_vals <- jaccard_mat[upper.tri(jaccard_mat)]
  avg_jaccard <- mean(jaccard_vals, na.rm = TRUE)
  
  graph <- graph_from_adjacency_matrix(jaccard_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  louvain <- cluster_louvain(graph, weights = E(graph)$weight)
  num_clusters <- length(unique(membership(louvain)))
  mod_score <- modularity(louvain)
  
  return(list(
    jaccard = round(avg_jaccard, 4),
    clusters = num_clusters,
    modularity = round(mod_score, 4),
    graph = graph,
    communities = louvain
  ))
}

# 🖼️ Visualize and save Louvain clusters
save_louvain_plot <- function(graph, communities, filename, label) {
  png(filename, width = 800, height = 600)
  plot(
    graph,
    vertex.color = communities$membership,
    vertex.label = V(graph)$name,
    vertex.size = 25,
    edge.width = E(graph)$weight * 5,
    layout = layout_with_fr,
    main = label
  )
  dev.off()
}

# 🚀 Main loop over all toy cases
toy_cases <- c("collaborative", "isolated", "2_clusters", "3_clusters", "3_clusters_overlap")
results <- data.frame()

for (case in toy_cases) {
  upa <- generate_toy_upa(case)
  
  # Save matrix as CSV
  write.csv(upa, file = file.path(output_dir, paste0("upa_", case, ".csv")), row.names = TRUE)
  
  analysis <- analyze_upa(upa, case)
  
  # Save Louvain plot
  plot_path <- file.path(output_dir, paste0("louvain_", case, ".png"))
  save_louvain_plot(analysis$graph, analysis$communities, plot_path, paste("Louvain –", case))
  
  # Append results
  results <- rbind(results, data.frame(
    Case = case,
    Jaccard = analysis$jaccard,
    Louvain_Clusters = analysis$clusters,
    Modularity = analysis$modularity
  ))
}

# 💾 Save summary table
write.csv(results, file = file.path(output_dir, "summary_metrics.csv"), row.names = FALSE)
print(results)
