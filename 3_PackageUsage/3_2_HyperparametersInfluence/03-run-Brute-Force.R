# 95 minutes on MacBook Air 2017, 4 cores
# 4 minutes on AMD EPYC 7413 Processor, 24 cores

DATADIR <- file.path(".", "3_PackageUsage", "3_1_breastCancer", "data")
source(file.path(DATADIR, "..", "02-define-functions.R"))

library(parallel)
numCores <- detectCores()
available_cores <- numCores

if (available_cores > nrow(tasks)){
  available_cores <- nrow(tasks) # more cores is useless, because mclapply will only take this many
}

task_ids <- 1:nrow(tasks)
# Parallelisation does not work on Windows
if (available_cores > 1) {
  task_results <- mclapply(task_ids, execute_task, mc.cores = available_cores)
} else {
  task_results <- lapply(task_ids, execute_task)
}

job_results_df_0 <- task_results
filename <- file.path(
  DATADIR,
  paste0(
    "job_0_results_",
    format(Sys.time(), "%H:%M:%S"), ".rda"
  )
)
save(job_results_df_0, file = filename)
