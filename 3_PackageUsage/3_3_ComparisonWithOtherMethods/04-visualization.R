library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# caption on plots
switch_names_structure_size <- function(structure_size_name) {
  ifelse(structure_size_name == "large",
    "large structure",
    ifelse(structure_size_name == "moderate",
      "moderate structure", "no structure"
    )
  )
}

# load data
DATADIR <- file.path(".", "3_PackageUsage", "3_3_ComparisonWithOtherMethods", "data")
files <- list.files(DATADIR, "*.rda")
files <- grep("job_[0-9]+_results_.*rda", files, value = TRUE)
l <- list()
for (i in 1:length(files)) {
  load(file.path(DATADIR, files[i]))
}
for (i in 1:length(files)) {
  l[[i]] <- get(paste0("job_results_df_", i - 1))
  remove(list = paste0("job_results_df_", i - 1))
}
task_results_df <- bind_rows(l)

# transform
task_results_df_1 <- task_results_df %>%
  rename(sample_size = n_points)

results_df <- task_results_df_1 %>%
  mutate(
    matrix_info = factor(matrix_structure, levels = c(
      "large_nozeros", "moderate_nozeros", "none_nozeros",
      "large_zeros", "moderate_zeros", "none_zeros"
    )),
    zeros_present = strsplit(as.character(matrix_info), "_") %>%
      sapply(function(v) v[2]),
    matrix_structure = strsplit(as.character(matrix_info), "_") %>%
      sapply(function(v) v[1]),
    avg_neg_loglik = loglikelihood / sample_size,
    sample_size = factor(sample_size)
  )

# It is possible that the package gave the estimator that was not proper.
# For example when `gips` run did provide an estimator with n0 < n.
# Fortunately, all of them did (We see the following matrix is full of 1).
results_df %>%
  group_by(algorithm, sample_size, matrix_structure, zeros_present) %>%
  summarise(positive_definite = mean(!is.infinite(loglikelihood))) %>%
  tidyr::pivot_wider(names_from = sample_size, values_from = positive_definite)

# The nonzeros looks very similar (to our surprise)
results_df_zeros <- results_df %>% 
  filter(zeros_present == "zeros")


# plots
plot_loglik_comparison <- results_df_zeros %>%
  ggplot(aes(x = algorithm, y = avg_neg_loglik, col = sample_size)) +
  geom_boxplot() +
  facet_wrap(vars(switch_names_structure_size(matrix_structure))) +
  labs(
    title = "Comparison between estimated and actual covariance matrix\nacross different matrix structures",
    col = "sample size (n)"
  ) +
  xlab("Estimating algoritm") +
  ylab("Negative loglikelihood weighted by sample size")
plot_loglik_comparison
ggsave(
  file.path(".", "plots", "Comparison_other_loglik.png"),
  plot_loglik_comparison,
  width = 20, height = 10,
  units = "cm"
)

plot_spectral_comparison <- results_df_zeros %>%
  ggplot(aes(x = algorithm, y = Frobenius_norm, col = sample_size)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10", breaks = c(1.25, 2.5, 5, 10, 20)) +
  facet_wrap(vars(switch_names_structure_size(matrix_structure))) +
  labs(
    title = "Comparison  between estimated and actual covariance matrix\nacross different matrix structures",
    col = "sample size (n)"
  ) +
  xlab("Estimating algoritm") +
  ylab("Frobenius norm")
plot_spectral_comparison
ggsave(
  file.path(".", "plots", "Comparison_other_Frobenius.png"),
  plot_spectral_comparison,
  width = 20, height = 10,
  units = "cm"
)
