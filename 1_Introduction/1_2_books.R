library(gips)
ggplot2::theme_set(ggplot2::theme_bw())

# This will add labels and numbers inside tiles
plot_cosmetic_modifications <- function(gg_plot_object) {
  my_col_names <- c("thick", "height", "breadth")

  suppressMessages( # message from ggplot2
    out <- gg_plot_object +
      ggplot2::scale_x_continuous(
        labels = my_col_names,
        breaks = 1:3
      ) +
      ggplot2::scale_y_reverse(
        labels = my_col_names,
        breaks = 1:3
      ) +
      ggplot2::theme(
        title = ggplot2::element_text(face = "bold", size = 18),
        legend.text = ggplot2::element_text(face = "bold", size = 10),
        axis.text.y = ggplot2::element_text(face = "bold", size = 17),
        axis.text.x = ggplot2::element_text(face = "bold", size = 17)
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#F0EA3E", mid = "#A41836", high = "#95E956",
        midpoint = 1.239099
      )
  )

  out +
    ggplot2::geom_text(ggplot2::aes(label = round(covariance, 1)),
      fontface = "bold", size = 8
    ) +
    ggplot2::theme(legend.position = "none")
}


##########
# Books

data("oddbooks", package = "DAAG")

head(oddbooks, 4)

Z <- oddbooks[, c(1, 2, 3)]

number_of_observations <- nrow(Z) # 12
p <- ncol(Z) # 3

Z$height <- Z$height / sqrt(2) # A-series paper size have a height / breadth = sqrt(2)

S <- cov(Z)
g <- gips(S, number_of_observations)
my_books_id_ggplot <- plot_cosmetic_modifications(plot(g, type = "heatmap")) +
  ggplot2::labs(
    title = "Standard MLE estimator",
    subtitle = "of the covariance matrix"
  )
my_books_id_ggplot
# Figure 3:
ggplot2::ggsave(
  file.path(".", "plots", "exp_books_id.png"),
  my_books_id_ggplot,
  width = 21.3, height = 18,
  units = "cm"
)


g_MAP <- find_MAP(g,
  optimizer = "brute_force", show_progress_bar = FALSE,
  return_probabilities = TRUE, save_all_perms = TRUE
)

get_probabilities_from_gips(g_MAP)
g_MAP

my_books_map_ggplot <- plot_cosmetic_modifications(plot(g_MAP, type = "heatmap"))
my_books_map_ggplot
# Figure 4:
ggplot2::ggsave(
  file.path(".", "plots", "exp_books_map.png"),
  my_books_map_ggplot,
  width = 21.3, height = 18,
  units = "cm"
)
