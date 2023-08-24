library(gips)
ggplot2::theme_set(ggplot2::theme_bw())

# This will add labels and numbers inside tiles
plot_cosmetic_modifications <- function(gg_plot_object) {
  my_col_names <- c(
    "deaths after placebo", "deaths after Aspirin",
    "treated with placebo", "treated with Aspirin"
  )

  suppressMessages( # message from ggplot2
    out <- gg_plot_object +
      ggplot2::scale_x_continuous(
        labels = my_col_names,
        breaks = 1:4
      ) +
      ggplot2::scale_y_reverse(
        labels = my_col_names,
        breaks = 1:4
      ) +
      ggplot2::theme(
        title = ggplot2::element_text(face = "bold", size = 18),
        legend.text = ggplot2::element_text(face = "bold", size = 10),
        axis.text.y = ggplot2::element_text(face = "bold", size = 11),
        axis.text.x = ggplot2::element_text(face = "bold", size = 11)
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#F0EA3E", mid = "#A41836", high = "#95E956",
        midpoint = 3172432
      )
  )

  out +
    ggplot2::geom_text(ggplot2::aes(label = round(covariance, -3)),
      fontface = "bold", size = 8
    ) +
    ggplot2::theme(legend.position = "none")
}


##########
# Aspirin

data("aspirin", package = "HSAUR2")

Z <- aspirin

# Renumber the columns for better readability:
Z[, c(2, 3)] <- Z[, c(3, 2)]
names(Z) <- names(Z)[c(1, 3, 2, 4)]
rownames(Z) <- NULL

head(Z, 4)

n <- nrow(Z) # 7
p <- ncol(Z) # 4

S <- cov(Z)

g <- gips(S, n)
my_aspirin_id_ggplot <- plot_cosmetic_modifications(plot(g, type = "heatmap")) +
  ggplot2::labs(
    title = "Standard MLE estimator",
    subtitle = "of the covariance matrix"
  )
my_aspirin_id_ggplot
# Figure 1:
ggplot2::ggsave(
  file.path(".", "plots", "exp_aspirin_id.png"),
  my_aspirin_id_ggplot,
  width = 24.4, height = 20.6,
  units = "cm"
)


g_MAP <- find_MAP(g,
  optimizer = "brute_force",
  save_all_perms = TRUE, return_probabilities = TRUE
)
g_MAP

get_probabilities_from_gips(g_MAP)

compare_posteriories_of_perms(g_MAP, "(34)")
compare_posteriories_of_perms(g_MAP, "(12)")
compare_posteriories_of_perms(g_MAP, "()")

S_projected <- project_matrix(S, g_MAP)

my_aspirin_perm_ggplot <- plot_cosmetic_modifications(plot(g_MAP, type = "heatmap"))
my_aspirin_perm_ggplot
# Figure 2:
ggplot2::ggsave(
  file.path(".", "plots", "exp_aspirin_perm.png"),
  my_aspirin_perm_ggplot,
  width = 24.4, height = 20.6,
  units = "cm"
)
