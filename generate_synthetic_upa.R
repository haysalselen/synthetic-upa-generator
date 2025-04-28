# === STEP 0: Load Required Libraries ===
# Make sure these are installed: install.packages(c("synthpop", "igraph", "Matrix", "proxy", "dplyr", "ggplot2"))
library(synthpop)
library(Matrix)
library(proxy)
library(igraph)
library(dplyr)
library(ggplot2)

# === STEP 1: Define the UPA Generator Function ===
generate_synthetic_upa <- function(U_real, n_prime, m_prime, alpha, zipf_influence = 0.5, zipf_exponent = 1.0) {
  
  # --- Step 1: User Synthesis ---
  df_real <- as.data.frame(U_real)
  df_real[] <- lapply(df_real, factor)
  
  syn_model <- syn(df_real, method = "cart", k = n_prime)
  U_prime <- as.matrix(syn_model$syn)
  U_prime <- apply(U_prime, 2, function(col) as.numeric(as.character(col)))
  
  # --- Step 2: Permission Space Adjustment ---
  perm_freq_real <- colSums(U_real)
  perm_freq_real_norm <- perm_freq_real / sum(perm_freq_real)
  m_orig <- ncol(U_real)
  m_diff <- m_prime - m_orig
  
  if (m_diff < 0) {
    keep_cols <- sample(names(perm_freq_real), m_prime, prob = perm_freq_real_norm)
    U_prime <- U_prime[, keep_cols, drop = FALSE]
  } else if (m_diff > 0) {
    zipf_weights <- 1 / ((1:m_diff)^zipf_exponent)
    zipf_weights <- zipf_weights / sum(zipf_weights)
    new_cols <- matrix(0, nrow = nrow(U_prime), ncol = m_diff)
    colnames(new_cols) <- paste0("Z", 1:m_diff)
    
    for (i in 1:m_diff) {
      template <- U_prime[, sample(1:ncol(U_prime), 1)]
      flips <- sample(1:length(template), floor(length(template) * 0.05))
      template[flips] <- 1 - template[flips]
      freq_zipf <- zipf_weights[i]
      mask <- rbinom(length(template), 1, prob = zipf_influence * freq_zipf + (1 - zipf_influence) * mean(template))
      new_cols[, i] <- mask * template
    }
    U_prime <- cbind(U_prime, new_cols)
  }
  
  # --- Step 3: Collaboration Calibration ---
  jaccard_sim <- function(mat) {
    mat <- as.matrix(mat)
    sim_matrix <- proxy::simil(mat, method = "Jaccard")
    sim_vals <- sim_matrix[lower.tri(sim_matrix)]
    if (all(is.na(sim_vals))) return(NA)
    return(mean(sim_vals, na.rm = TRUE))
  }
  
  calculate_louvain <- function(mat) {
    sim <- as.matrix(proxy::simil(mat, method = "Jaccard"))
    sim[is.na(sim)] <- 0
    g <- graph_from_adjacency_matrix(sim, mode = "undirected", weighted = TRUE, diag = FALSE)
    if (gsize(g) == 0) return(list(modularity = NA, count = NA))
    lc <- cluster_louvain(g)
    return(list(modularity = modularity(lc), count = length(unique(membership(lc)))))
  }
  
  current_collab <- jaccard_sim(U_prime)
  if (!is.na(current_collab) && alpha > current_collab) {
    shared_col <- rbinom(nrow(U_prime), 1, prob = 0.5)
    shared_matrix <- matrix(rep(shared_col, m_prime), nrow = nrow(U_prime))
    U_prime <- pmin(U_prime + round(shared_matrix * (alpha - current_collab)), 1)
  } else if (!is.na(current_collab) && alpha < current_collab) {
    mask <- matrix(runif(length(U_prime)), nrow = nrow(U_prime))
    U_prime[mask < (current_collab - alpha) * 0.5] <- 0
  }
  
  # --- Step 4: Metrics ---
  j_real <- jaccard_sim(U_real)
  j_synth <- jaccard_sim(U_prime)
  mod_real <- calculate_louvain(U_real)
  mod_synth <- calculate_louvain(U_prime)
  
  cat("=== Metric Comparison ===\n")
  cat("Real Jaccard:", round(j_real, 4), "| Synthetic Jaccard:", round(j_synth, 4), "\n")
  cat("Real Modularity:", round(mod_real$modularity, 4), "| Synthetic Modularity:", round(mod_synth$modularity, 4), "\n")
  cat("Real Cluster Count:", mod_real$count, "| Synthetic Cluster Count:", mod_synth$count, "\n")
  
  # --- Step 5: Frequency Plot (w/ white background) ---
  real_df <- data.frame(perm = factor(names(colSums(U_real)), levels = names(colSums(U_real))),
                        freq = as.numeric(colSums(U_real)),
                        type = "Real")
  synth_df <- data.frame(perm = factor(colnames(U_prime), levels = unique(c(colnames(U_real), colnames(U_prime)))),
                         freq = as.numeric(colSums(U_prime)),
                         type = "Synthetic")
  full_df <- rbind(real_df, synth_df)
  
  plot <- ggplot(full_df, aes(x = perm, y = freq, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Permission Frequencies: Real vs Synthetic") +
    xlab("Permission") + ylab("Frequency (Number of Users)") +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  # --- Step 6: Louvain Side-by-Side ---
  plot_louvain_graph <- function(mat, title) {
    sim <- proxy::simil(mat, method = "Jaccard")
    sim[is.na(sim)] <- 0
    g <- graph_from_adjacency_matrix(as.matrix(sim), mode = "undirected", weighted = TRUE, diag = FALSE)
    if (gsize(g) == 0) {
      cat("Graph for", title, "is empty. Skipping plot.\n")
      return()
    }
    lc <- cluster_louvain(g)
    plot(
      g,
      vertex.label = NA,
      vertex.color = membership(lc),
      main = title,
      layout = layout_with_fr,
      edge.width = E(g)$weight * 5,
      vertex.size = 5
    )
  }
  
  if (!dir.exists("output")) dir.create("output")
  
  param_str <- paste0(
    "n", n_prime, "_m", m_prime,
    "_a", gsub("\\.", "p", sprintf("%.2f", alpha)),
    "_zi", gsub("\\.", "p", sprintf("%.2f", zipf_influence)),
    "_ze", gsub("\\.", "p", sprintf("%.2f", zipf_exponent))
  )
  
  ggsave(
    filename = paste0("output/freqplot_", param_str, ".png"),
    plot = plot, width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  png(filename = paste0("output/louvain_", param_str, ".png"), width = 1200, height = 600)
  par(mfrow = c(1, 2))
  plot_louvain_graph(U_real, "Louvain Clustering: Real")
  plot_louvain_graph(U_prime, "Louvain Clustering: Synthetic")
  par(mfrow = c(1, 1))
  dev.off()
  
  # --- Step 7: Logging ---
  log_file <- "output/synthesis_log.csv"
  log_entry <- data.frame(
    timestamp = Sys.time(),
    n_prime = n_prime,
    m_prime = m_prime,
    alpha = alpha,
    zipf_influence = zipf_influence,
    zipf_exponent = zipf_exponent,
    jaccard_real = round(j_real, 4),
    jaccard_synth = round(j_synth, 4),
    modularity_real = round(mod_real$modularity, 4),
    modularity_synth = round(mod_synth$modularity, 4),
    cluster_count_real = mod_real$count,
    cluster_count_synth = mod_synth$count
  )
  
  if (!file.exists(log_file)) {
    write.csv(log_entry, log_file, row.names = FALSE)
  } else {
    write.table(log_entry, log_file, row.names = FALSE, col.names = FALSE, sep = ",", append = TRUE)
  }
  
  return(structure(U_prime,
                   jaccard = round(j_synth, 4),
                   modularity = round(mod_synth$modularity, 4),
                   clusters = mod_synth$count))
  
}
