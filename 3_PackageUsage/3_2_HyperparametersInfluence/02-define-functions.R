library(gips)
library(magrittr)

show_progress_bar <- TRUE

DATADIR <- file.path(".", "3_PackageUsage", "3_1_breastCancer", "data")
load(file.path(DATADIR, "matrices_8.rda"))

p <- nrow(cov_large_str)
n_points <- 30
d_list <- c(0.1, 1, 10, 100)
delta_list <- c(3, 30)
structures <- c("large", "moderate", "no")

set.seed(2022)


# Make S_large, S_moderate and S_no:
for (struc in structures) {
  Sigma <- get(paste0("cov_", struc, "_str"))
  Z <- MASS::mvrnorm(
    n_points,
    mu = rep(0, p),
    Sigma = Sigma
  )
  S <- (t(Z) %*% Z) / n_points # we know the mean
  assign(paste0("S_", struc), S)
}

tasks <- expand.grid(
  d = d_list,
  delta = delta_list,
  matrix_structure = structures
)

get_n_dim_from_perm_name <- function(g_perm_name, size = p) {
  g_perm <- gips_perm(g_perm_name, size = size)
  n_dim <- sum(get_structure_constants(g_perm)[["dim_omega"]])
  
  n_dim
}
get_n0_from_perm_name <- function(g_perm_name, size = p) {
  structure_constants <- get_structure_constants(gips_perm(g_perm_name, p))
  n0 <- max(structure_constants[["r"]] * structure_constants[["d"]] / structure_constants[["k"]])
  
  n0
}

conduct_trial <- function(d, delta, matrix_structure) {
  S <- get(paste0("S_", matrix_structure))
  D_matrix <- diag(ncol(S)) * d

  g <- gips(S, n_points,
    delta = delta, D_matrix = D_matrix,
    was_mean_estimated = FALSE
  )
  g_MAP <- find_MAP(g,
    optimizer = "BF",
    show_progress_bar = show_progress_bar,
    save_all_perms = TRUE,
    return_probabilities = TRUE
  )
  probs <- get_probabilities_from_gips(g_MAP)
  
  # n_parameters
  visited_n_dim <- sapply(
    names(probs),
    get_n_dim_from_perm_name
  ) %>% factor()
  n_dim_distribution <- split(probs, visited_n_dim) %>%
    sapply(sum) %>%
    setNames(attr(visited_n_dim, "levels"))
  
  # n0
  visited_n0 <- sapply(
    names(probs),
    get_n0_from_perm_name
  ) %>% factor()
  n0_distribution <- split(probs, visited_n0) %>%
    sapply(sum) %>%
    setNames(attr(visited_n0, "levels"))

  list("n_dim_distribution" = n_dim_distribution,
       "n0_distribution" = n0_distribution)
}


execute_task <- function(task_id) {
  # set.seed is not needed, because the computations are deterministic
  task_info <- as.vector(tasks[task_id, ])
  delta <- task_info[["delta"]]
  d <- task_info[["d"]]
  matrix_structure <- task_info[["matrix_structure"]]
  trial_results <- conduct_trial(d, delta, matrix_structure)
  out_list <- as.list(tasks[task_id, ])
  attr(out_list, "out.attrs") <- NULL
  out_list[["task_id"]] <- task_id
  out_list[["n_dim_distribution"]] <- trial_results[["n_dim_distribution"]]
  out_list[["n0_distribution"]] <- trial_results[["n0_distribution"]]
  out_list
}
