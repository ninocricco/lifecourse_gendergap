#------------------------------------------------------------------------------
# PROJECT: GENDER PAY GAP STARTING POINTS AND LIFE COURSE DIVERGENCE
# FILE: COMBINING BOOTSTRAPPED ESTIMATES (REFACTORED)
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------
library(data.table)

# 1) Directory containing chunk files
chunk_dir <- "bootstrap_estimates/main"

all_files <- list.files(
  path       = chunk_dir,
  pattern    = "^main_bootstrap_\\d+\\.rds$",
  full.names = TRUE
)

chunk_ids <- as.integer(
  sub(".*/main_bootstrap_(\\d+)\\.rds$", "\\1", all_files)
)
ord <- order(chunk_ids)
files_ordered <- all_files[ord]

total_iters <- 10000
master_list1 <- vector("list", total_iters)
master_list2 <- vector("list", total_iters)
master_list3 <- vector("list", total_iters)

# 5) Iterate over each chunk file and fill the master lists
counter <- 1
for (f in files_ordered) {
  chunk_data <- readRDS(f)
  # chunk_data is assumed to be a list of length iters_per_chunk (e.g. 8)
  # and each chunk_data[[i]] itself is a list of 3 data.frames.
  if (!is.list(chunk_data)) {
    stop("File ", f, " did not return a list of iterations.")
  }
  for (iter_item in chunk_data) {
    if (!is.list(iter_item) || length(iter_item) < 3) {
      stop("In file ", f, ": each iteration must be a list of 3 data.frames.")
    }
    # Place the 3 data.frames into the corresponding master lists:
    master_list1[[counter]] <- iter_item[[1]]
    master_list2[[counter]] <- iter_item[[2]]
    master_list3[[counter]] <- iter_item[[3]]
    counter <- counter + 1
  }
}

master_list1 <- rbindlist(master_list1)
write_rds(master_list1, "bootstrap_estimates/main/fig1_bootstrapped_estimates.rds")
master_list2 <- rbindlist(master_list2)
write_rds(master_list2, "bootstrap_estimates/main/fig2_bootstrapped_estimates.rds")
master_list3 <- rbindlist(master_list3)
write_rds(master_list3, "bootstrap_estimates/main/fig3_bootstrapped_estimates.rds")
