library(gips)
library(huge)
library(rags2ridges) # requires installing dependencies from BioConductor

N_TRIAL <- 10
GIPS_N_ITER <- 300000
DATADIR <- file.path(".", "3_PackageUsage", "3_3_ComparisonWithOtherMethods", "data")

METHODS <- c("rags2ridges", "huge", "gips")

load(file.path(DATADIR, "matrices_50.rda"))
p <- nrow(cov_large_str_no_zeros)
DEFAULT_D_MATRIX <- diag(p)

tasks <- expand.grid(
  matrix_structure = factor(c(
    "none_zeros", "none_nozeros", "moderate_zeros", "moderate_nozeros",
    "large_zeros", "large_nozeros"
  )),
  n_points = c(10, 20, 40),
  n_trial = 1:N_TRIAL
)

execute_task <- function(task_id) {
  set.seed(task_id)
  task_info <- as.vector(tasks[task_id, ])
  true_cov <- switch(as.character(task_info[["matrix_structure"]]),
    none_zeros = cov_no_str_zeros,
    none_nozeros = cov_no_str_no_zeros,
    moderate_zeros = cov_mod_str_zeros,
    moderate_nozeros = cov_mod_str_no_zeros,
    large_zeros = cov_large_str_zeros,
    large_nozeros = cov_large_str_no_zeros
  )
  n_points <- as.numeric(as.character(task_info[["n_points"]]))
  trial_results <- conduct_trial(true_cov, n_points, task_id)
  cbind(trial_results, tasks[task_id, ], data.frame(task_id = task_id))
}

conduct_trial <- function(true_cov, n_points, task_id = -1) {
  withr::with_seed(
    task_id,
    Z <- MASS::mvrnorm(
      n_points,
      mu = rep(0, nrow(true_cov)), Sigma = true_cov
    )
  )
  estimated_covs <- lapply(METHODS, function(algorithm) {
    estimate_covariance(Z, n_points, algorithm, task_id)
  })
  loglikelihoods <- sapply(estimated_covs, function(estimated_cov) {
    calculate_loglikelihood(Z, estimated_cov)
  })
  Frobenius_norms <- sapply(estimated_covs, function(estimated_cov) {
    calculate_Frobenius_Norm(true_cov, estimated_cov)
  })
  data.frame(
    algorithm = METHODS,
    loglikelihood = loglikelihoods,
    Frobenius_norm = Frobenius_norms
  )
}

estimate_covariance <- function(Z, n_points, method = "", task_id = -1) {
  print(paste0("Began estimating covariance with method ", method, " at ", Sys.time()))
  out <- switch(method,
    gips = estimate_covariance_with_gips(Z, n_points, task_id),
    rags2ridges = estimate_covariance_with_ridge(Z, n_points),
    huge = estimate_covariance_with_huge(Z, n_points)
  )
  print(paste0("Finished estimating covariance with method ", method, " at ", Sys.time()))
  out
}

estimate_covariance_with_gips <- function(Z, n_points, task_id) {
  S <- (t(Z) %*% Z) / n_points # we know the mean is 0
  g <- gips(S, n_points, was_mean_estimated = FALSE)
  g_MAP <- find_MAP(g,
    max_iter = GIPS_N_ITER, optimizer = "MH",
    save_all_perms = FALSE
  )
  filename <- file.path(DATADIR, "tasks_results", paste0(
    "task_", task_id, "_gMAP_",
    format(Sys.time(), "%H:%M:%S"), ".rda"
  ))
  save(g_MAP, file = filename)
  print(g_MAP)
  project_matrix(S, g_MAP[[1]])
}

estimate_covariance_with_ridge <- function(Z, n_points) {
  S <- cov(Z)
  target <- solve(DEFAULT_D_MATRIX)
  opt <- optPenalty.kCVauto(Z,
    lambdaMin = 0.001, lambdaMax = 100, # values from README
    target = target
  )
  optLambda <- opt[["optLambda"]]
  precisionM <- ridgeP(S, optLambda, target = target)
  solve(precisionM)
}

estimate_covariance_with_huge <- function(Z, n_points) {
  huge_obj <- huge(Z, # huge will estimate the mean
    nlambda = 40, lambda.min.ratio = 0.02, # values from paper
    method = "glasso", cov.output = TRUE
  )
  select_obj <- huge.select(huge_obj)
  select_obj$opt.cov
}

calculate_loglikelihood <- function(Z, est_cov) {
  loglikelihoods <- mvtnorm::dmvnorm(Z, sigma = est_cov, log = TRUE)
  -sum(loglikelihoods)
}

calculate_Frobenius_Norm <- function(true_cov, est_cov) {
  my_diff <- (true_cov - est_cov)
  sqrt(sum(abs(my_diff)^2))
}
