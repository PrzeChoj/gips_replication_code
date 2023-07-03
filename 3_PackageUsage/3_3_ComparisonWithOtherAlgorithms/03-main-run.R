# 1 h 43 min on AMD EPYC 7413 Processor, 40 cores

source(file.path(".", "3_PackageUsage", "3_3_ComparisonWithOtherAlgorithms", "02-setup-and-functions.R"))

library(parallel)
numCores <- detectCores()
expected_cores <- 40
stopifnot(numCores >= expected_cores)

task_ids <- 1:nrow(tasks) # 180 tasks
# Parallelisation does not work on Windows
if (expected_cores > 1) {
  task_results <- mclapply(task_ids, execute_task, mc.cores = expected_cores)
} else {
  task_results <- lapply(task_ids, execute_task)
}

job_results_df_0 <- do.call(rbind, task_results)
filename <- file.path(
  DATADIR,
  paste0(
    "job_0_results_",
    format(Sys.time(), "%H:%M:%S"), ".rda"
  )
)
save(job_results_df_0, file = filename)
