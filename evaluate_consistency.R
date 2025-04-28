# === STEP 0: Setup ===
library(synthpop)
library(Matrix)
library(proxy)
library(igraph)
library(dplyr)
library(ggplot2)

# === STEP 1: Define the Experiment Function ===
run_full_evaluation <- function(upa_file, output_filename) {
  cat(sprintf("\n📂 Running evaluation for dataset: %s\n", upa_file))
  
  # === Load Real UPA Matrix ===
  U_real <- as.matrix(read.table(upa_file))
  U_real <- apply(U_real, 2, as.numeric)
  n_real <- nrow(U_real)
  m_real <- ncol(U_real)
  
  cat(sprintf("🔢 Real dataset has %d users and %d permissions.\n", n_real, m_real))
  
  # === Parameter Grids ===
  user_counts <- c(round(n_real / 2), n_real, n_real * 2)
  perm_counts <- c(round(m_real / 2), m_real, m_real * 2)
  alphas <- c(0.1, 0.5, 0.9)
  repeats <- 50
  
  all_results <- list()
  
  # === Running the Experiments ===
  for (n in user_counts) {
    for (m in perm_counts) {
      for (a in alphas) {
        for (i in 1:repeats) {
          cat(sprintf("[Trial %02d] U = %d | P = %d | α = %.1f\n", i, n, m, a))
          
          result <- generate_synthetic_upa(
            U_real = U_real,
            n_prime = n,
            m_prime = m,
            alpha = a,
            zipf_influence = 0.5,
            zipf_exponent = 1.0
          )
          
          all_results[[length(all_results) + 1]] <- data.frame(
            alpha = a,
            user_count = n,
            perm_count = m,
            repetition = i,
            jaccard = attr(result, "jaccard"),
            modularity = attr(result, "modularity"),
            clusters = attr(result, "clusters")
          )
        }
      }
    }
  }
  
  df_all <- do.call(rbind, all_results)
  write.csv(df_all, output_filename, row.names = FALSE)
  cat(sprintf("\n✅ Completed runs for %s! Results saved to %s\n", upa_file, output_filename))
}

# === STEP 2: Load Your Generator ===
source("generate_synthetic_upa.R")

# === STEP 3: Call the Function for Any Dataset ===
# Example for Firewall1:
# run_full_evaluation("Firewall1-UPA.txt", "output/full_evaluation_firewall1.csv")
