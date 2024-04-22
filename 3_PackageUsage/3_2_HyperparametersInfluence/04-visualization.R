library(ggplot2)
library(dplyr)
theme_set(theme_bw())

# load data
DATADIR <- file.path(".", "3_PackageUsage", "3_2_HyperparametersInfluence", "data")
load(file.path(DATADIR, "matrices_8.rda"))
files <- list.files(DATADIR, "*.rda")
files <- grep("job_[0-9]+_results_.*rda", files, value = TRUE)
stopifnot(length(files) == 1)
l <- list()
for (i in 1:length(files)) {
  load(file.path(DATADIR, files[i]))
}
for (i in 1:length(files)) {
  l[[i]] <- get(paste0("job_results_df_", i - 1))
  remove(list = paste0("job_results_df_", i - 1))
}
job_results <- unlist(l, recursive = FALSE)

# n_dim Distribution
n_dim_df <- lapply(job_results, function(el) {
  n_dim <- names(el$n_dim_distribution)
  n_dim_prob <- el$n_dim_distribution %>% setNames(NULL)
  el$n_dim_distribution <- NULL
  el$n0_distribution <- NULL
  as.data.frame(el) %>% cbind(data.frame(
    n_dim = n_dim,
    n_dim_prob = n_dim_prob
  ))
}) %>% bind_rows()
# n0 Distribution
n0_df <- lapply(job_results, function(el) {
  n0 <- names(el$n0_distribution)
  n0_prob <- el$n0_distribution %>% setNames(NULL)
  el$n_dim_distribution <- NULL
  el$n0_distribution <- NULL
  as.data.frame(el) %>% cbind(data.frame(
    n0 = n0,
    n0_prob = n0_prob
  ))
}) %>% bind_rows()

n0_df_forplot <- mutate(n0_df,
                        x_elements = as.numeric(as.character(n0)),
                        y_elements = n0_prob)
n_dim_df_forplot <- mutate(n_dim_df,
                           x_elements = as.numeric(as.character(n_dim)),
                           y_elements = n_dim_prob)

plot_heatmap_true <- function(my_ggplot){
  my_ggplot +
    labs(fill = "covariance") +
    theme(
      title = element_text(size = 16),
      legend.title = element_text(size = 16)
    )
}

plot_for_matrix_structure <- function(my_matrix_structure, x_axis_type, hide_legend, title) {
  stopifnot(x_axis_type %in% c("n0", "n_dim"))

  if (x_axis_type == "n0"){
    my_df <- n0_df_forplot
    true_line <- ifelse(my_matrix_structure == "large", 1,
                        ifelse(my_matrix_structure == "moderate", 5,
                               ifelse(my_matrix_structure == "no", 8, NA)))
    my_xlabel <- "Simplicyty of the model (n0)"
    x_breaks <- 1:8
  } else {
    my_df <- n_dim_df_forplot
    true_line <- ifelse(my_matrix_structure == "large", 5,
                        ifelse(my_matrix_structure == "moderate", 17,
                               ifelse(my_matrix_structure == "no", 36, NA)))
    my_xlabel <- "Number of parameters in a model (n_dim)"
    x_breaks <-
      if (my_matrix_structure == "moderate") {
        c(5, 10, 17, 20, 30, 36) # additional 17
      } else {
        c(5, 10,     20, 30, 36)
      }
  }

  out_plot <- filter(
    my_df,
    matrix_structure == my_matrix_structure
  ) %>%
    ggplot(aes(x = x_elements, y = y_elements, fill = factor(delta))) +
    geom_col(position = "dodge", width = 0.7) +
    geom_vline(xintercept = true_line, linetype = "dashed") +
    facet_wrap(~d, nrow = 2, labeller = "label_both") +
    xlab(my_xlabel) +
    ylab("Probability") +
    guides(fill = guide_legend(title = "delta", override.aes = aes(alpha = 1))) +
    labs(title = title) +
    lims(y = c(0, 1)) +
    scale_x_continuous(breaks = x_breaks) +
    theme(title = element_text(size = 15))

  if (hide_legend){
    out_plot <- out_plot +
      theme(legend.position = "none")
  }

  out_plot
}

my_heatmap_true_large <- plot_heatmap_true(gips:::pretty_plot_matrix(cov_large_str,
  bquote(atop("Heatmap of the true covariance matrix", bold("Large structure                                  ")))))
my_heatmap_true_moderate <- plot_heatmap_true(gips:::pretty_plot_matrix(cov_moderate_str,
  bquote(atop("Heatmap of the true covariance matrix", bold("Moderate structure                            ")))))
my_heatmap_true_no <- plot_heatmap_true(gips:::pretty_plot_matrix(cov_no_str,
  bquote(atop("Heatmap of the true covariance matrix", bold("No structure                                       ")))))

# my_heatmap_true_large
# my_heatmap_true_moderate
# my_heatmap_true_no
ggsave(
  file.path(".", "plots", "hyper_params_influence_true_large.png"),
  my_heatmap_true_large,
  width = 13, height = 11.3,
  units = "cm"
)
ggsave(
  file.path(".", "plots", "hyper_params_influence_true_moderate.png"),
  my_heatmap_true_moderate,
  width = 13, height = 11.3,
  units = "cm"
)
ggsave(
  file.path(".", "plots", "hyper_params_influence_true_no.png"),
  my_heatmap_true_no,
  width = 13, height = 11.3,
  units = "cm"
)

structures <- c("no", "moderate", "large")
hide_legend <- c(TRUE, TRUE, FALSE)
plot_width <- c(7, 7, 7.8)
x_axis_type <- "n_dim"

ggsave(
  file.path(".", "plots", paste0("hyper_params_influence_", x_axis_type, "_", structures[1], ".png")),
  plot_for_matrix_structure(structures[1], x_axis_type, hide_legend[1],
    title = bquote(atop("Effect of parameters on a posteriori structure distribution",
                        "for a matrix with" ~ bold("no") ~ "structure                                            "))),
  width = plot_width[1]
)
ggsave(
  file.path(".", "plots", paste0("hyper_params_influence_", x_axis_type, "_", structures[2], ".png")),
  plot_for_matrix_structure(structures[2], x_axis_type, hide_legend[2],
    title = bquote(atop("Effect of parameters on a posteriori structure distribution",
                        "for a matrix with" ~ bold("moderate") ~ "structure                                "))),
  width = plot_width[2]
)
ggsave(
  file.path(".", "plots", paste0("hyper_params_influence_", x_axis_type, "_", structures[3], ".png")),
  plot_for_matrix_structure(structures[3], x_axis_type, hide_legend[3],
    title = bquote(atop("Effect of parameters on a posteriori structure distribution",
                        "for a matrix with" ~ bold("large") ~ "structure                                       "))),
  width = plot_width[3]
)
