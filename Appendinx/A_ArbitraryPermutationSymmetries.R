library(gips)

p <- 4
n <- 50

set.seed(2022)
Z <- matrix(rnorm(n * p), ncol = p)
S <- cov(Z)

g <- gips(S, n)
g_MAP <- find_MAP(g,
  optimizer = "BF", show_progress_bar = FALSE,
  return_probabilities = TRUE, save_all_perms = TRUE
)
get_probabilities_from_gips(g_MAP)
