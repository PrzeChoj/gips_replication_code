library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# for caption on plots
switch_names_structure_size <- function(structure_size_name) {
  ifelse(structure_size_name == "large",
    "large structure",
    ifelse(structure_size_name == "moderate",
      "moderate structure", "no structure"
    )
  )
}

# load data
dirr <- file.path(".", "3_PackageUsage", "3_3_ComparisonWithOtherAlgorithms", "data")
files <- list.files(dirr, "*.rda")
files <- grep("job_[0-9]+_results_.*rda", files, value = TRUE)
l <- list()
for (i in 1:length(files)) {
  load(file.path(dirr, files[i]))
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
# Fortunately, all of them did (We see the following matrix is full of 1). This is awesome.
results_df %>%
  group_by(algorithm, sample_size, matrix_structure, zeros_present) %>%
  summarise(positive_definite = mean(!is.infinite(loglikelihood))) %>%
  tidyr::pivot_wider(names_from = sample_size, values_from = positive_definite)

# In the nonzeros looks very similar (to our rurprise)
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

###### Some other, old plots
results_df %>%
  mutate(sample_size = factor(sample_size)) %>%
  ggplot(aes(x = algorithm, y = avg_neg_loglik, col = sample_size)) +
  geom_boxplot() +
  facet_wrap(~matrix_info, scales = "free") +
  labs(title = "Comparison  between estimated and actual covariance matrix\nacross different matrix structures") +
  xlab("Estimating algoritm") +
  ylab("Negative loglikelihood weighted by sample size")

results_df %>%
  filter(sample_size == 20) %>%
  mutate(sample_size = factor(sample_size)) %>%
  ggplot(aes(x = algorithm, y = spectral_distance, col = algorithm)) +
  geom_boxplot() +
  facet_wrap(~matrix_info, scales = "free") +
  labs(title = "Comparison  between estimated and actual covariance matrix\nfor sample size = 20") +
  xlab("Estimating algoritm") +
  ylab("Spectral distance")

transformed_df <-
  group_by(results_df, sample_size, algorithm, matrix_info) %>%
  summarise(
    mean_m_1 = mean(spectral_distance),
    sd_m_1 = sd(spectral_distance)
  ) %>%
  mutate(
    min_m_1 = mean_m_1 - 2 * sd_m_1,
    min_m_1 = if_else(min_m_1 > 0, min_m_1, 0),
    max_m_1 = mean_m_1 + 2 * sd_m_1
  )

transformed_df %>%
  filter(sample_size != 5) %>%
  ggplot(aes(
    x = sample_size, y = mean_m_1,
    ymin = min_m_1, ymax = max_m_1
  )) +
  geom_line(aes(col = algorithm)) +
  geom_ribbon(aes(fill = algorithm), alpha = 0.1) +
  facet_wrap(~matrix_info, scales = "free") +
  labs(title = "Comparison between estimated and actual covariance matrix\nacross matrices and sample sizes") +
  xlab("Sample size") +
  ylab("Spectral distance")

group_by(results_df, sample_size, algorithm, matrix_info) %>%
  filter(sample_size != 5) %>%
  summarise(
    mean_m = mean(avg_neg_loglik),
    sd_m = sd(avg_neg_loglik)
  ) %>%
  mutate(
    min_m = mean_m - 2 * sd_m,
    max_m = mean_m + 2 * sd_m
  ) %>%
  ggplot(aes(
    x = sample_size, y = mean_m,
    ymin = min_m, ymax = max_m
  )) +
  geom_line(aes(col = algorithm)) +
  geom_ribbon(aes(fill = algorithm), alpha = 0.1) +
  facet_wrap(~matrix_info, scales = "free") +
  labs(title = "Comparison between estimated and actual covariance matrix\nacross matrices and sample sizes") +
  xlab("Sample size") +
  ylab("Spectral distance")
