DATADIR <- file.path(".", "3_PackageUsage", "3_3_ComparisonWithOtherMethods", "data")

change_no_zeros_to_zeros <- function(no_zeros_matrix) {
  with_zeros_matrix <- no_zeros_matrix
  cutoff_val <- quantile(abs(with_zeros_matrix), 0.25)
  with_zeros_matrix[abs(with_zeros_matrix) < cutoff_val] <- 0

  with_zeros_matrix
}

symmetric_solve <- function(symmetric_matrix) {
  solved_symmetric_matrix <- solve(symmetric_matrix)
  (solved_symmetric_matrix + t(solved_symmetric_matrix)) / 2 # this averaging makes almost no difference
}


p <- 50 # Dimensions of created covariance matrices.
n <- 50

set.seed(2022)

Z <- MASS::mvrnorm(n = n, mu = numeric(p), Sigma = diag(1, p))
S <- (t(Z) %*% Z) / p

# Permutations for structures
large_structure_perm <- paste0("(", toString(1:50), ")")
moderate_structure_perm <- paste0("(", toString(1:25), ")")


# large structure, no zeroes
cov_large_str_no_zeros <- gips::project_matrix(S, large_structure_perm)
prec_large_str_no_zeros <- symmetric_solve(cov_large_str_no_zeros)


# large structure, zeroes
prec_large_str_zeros <- change_no_zeros_to_zeros(prec_large_str_no_zeros)
cov_large_str_zeros <- symmetric_solve(prec_large_str_zeros)


# moderate structure, no zeroes
cov_mod_str_no_zeros <- gips::project_matrix(S, moderate_structure_perm)
prec_mod_str_no_zeros <- symmetric_solve(cov_mod_str_no_zeros)


# moderate_structure, zeroes
prec_mod_str_zeros <- change_no_zeros_to_zeros(prec_mod_str_no_zeros)
cov_mod_str_zeros <- symmetric_solve(prec_mod_str_zeros)


# no structure no zeroes
cov_no_str_no_zeros <- S + 0.1 * diag(p) # a correction to ensure positive definitness of prec_no_str_zeros
prec_no_str_no_zeros <- symmetric_solve(cov_no_str_no_zeros)


# no structure, zeroes
prec_no_str_zeros <- change_no_zeros_to_zeros(prec_no_str_no_zeros)
cov_no_str_zeros <- symmetric_solve(prec_no_str_zeros)

save(cov_large_str_no_zeros, cov_large_str_zeros, cov_mod_str_no_zeros,
  cov_mod_str_zeros, cov_no_str_no_zeros, cov_no_str_zeros,
  file = file.path(DATADIR, paste0("matrices_", p, ".rda"))
)
