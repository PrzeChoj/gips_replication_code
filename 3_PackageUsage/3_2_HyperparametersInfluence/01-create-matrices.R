library(gips)

set.seed(2022)

DATADIR <- file.path(".", "3_PackageUsage", "3_1_breastCancer", "data")

p <- 8
large_structure_perm <- "(12345678)"
moderate_structure_perm <- "(1234)"

Z <- MASS::mvrnorm(n = p, mu = numeric(p), Sigma = diag(1, p))
S <- (t(Z) %*% Z) / p


#########
# cov_large_str creation
cov_large_str <- project_matrix(S, large_structure_perm)

#########
# cov_moderate_str creation
cov_moderate_str <- project_matrix(S, moderate_structure_perm)

#########
# cov_no_str creation
cov_no_str <- S


#########
# save
save(cov_large_str, cov_moderate_str, cov_no_str,
  file = file.path(DATADIR, paste0("matrices_", p, ".rda"))
)
