library(gips)
library(MASS) # for mvrnorm()
library(ggplot2)

p <- 5
n <- 10

get_plotted_matrix <- function(my_matrix, my_title) {
  suppressMessages( # message from ggplot2
    gips:::pretty_plot_matrix(my_matrix, title = my_title) +
      geom_text(
        aes(label = round(covariance, 1)),
        fontface = "bold", size = 8
      ) +
      labs(fill = "covariance") +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 25),
      ) +
      scale_fill_gradient2(
        low = "#F0EA3E", mid = "#A41836", high = "#95E956",
        midpoint = 8.660486
    )
  )
}


# Section 2.1

set.seed(2022)
sigma_matrix <- matrix(
  data = c(
    10, 08, 06, 06, 08,
    08, 10, 08, 06, 06,
    06, 08, 10, 08, 06,
    06, 06, 08, 10, 08,
    08, 06, 06, 08, 10
  ),
  nrow = p, byrow = TRUE
) # sigma_matrix is a matrix invariant under permutation (1,2,3,4,5)
Z <- mvrnorm(n, mu = rep(0, p), Sigma = sigma_matrix)
S <- cov(Z)


# Plots

# full symmetry
off_diagonal_element <- mean(S[row(S) != col(S)])
on_diagonal_element <- mean(S[row(S) == col(S)])
S_full <- matrix(rep(off_diagonal_element, p * p), nrow = p) + diag(on_diagonal_element - off_diagonal_element, p)
my_full_symmetry_ggplot <- get_plotted_matrix(
  S_full,
  bquote(atop("Example of a matrix" ~ bold("with full"), "permutation symmetry           "))
)
my_full_symmetry_ggplot
# Figure 5 top left:
ggplot2::ggsave(
  file.path(".", "plots", "Methodological_Background_full_symmetry.png"),
  my_full_symmetry_ggplot,
  width = 16, height = 18,
  units = "cm"
)

# long perm symmetry
S_long <- project_matrix(S, "(12345)")

my_full_long_perm_ggplot <- get_plotted_matrix(
  S_long,
  bquote(atop("Example of a matrix" ~ bold("with long"), "permutation symmetry             "))
)
my_full_long_perm_ggplot
# Figure 5 top right:
ggplot2::ggsave(
  file.path(".", "plots", "Methodological_Background_long_perm.png"),
  my_full_long_perm_ggplot,
  width = 16, height = 18,
  units = "cm"
)

# short perm symmetry
S_short <- project_matrix(S, "(123)")

my_short_perm_ggplot <- get_plotted_matrix(
  S_short,
  bquote(atop("Example of a matrix" ~ bold("with short"), "permutation symmetry              "))
)
my_short_perm_ggplot
# Figure 5 bottom left:
ggplot2::ggsave(
  file.path(".", "plots", "Methodological_Background_short_perm.png"),
  my_short_perm_ggplot,
  width = 16, height = 18,
  units = "cm"
)

# no symmetry
my_no_perm_ggplot <- get_plotted_matrix(
  S,
  bquote(atop("Example of a matrix" ~ bold("without"), "permutation symmetry          "))
)
my_no_perm_ggplot
# Figure 5 bottom right:
ggplot2::ggsave(
  file.path(".", "plots", "Methodological_Background_no_perm.png"),
  my_no_perm_ggplot,
  width = 16, height = 18,
  units = "cm"
)


# Section 2.2
g <- gips(S, n, perm = "(12345)", was_mean_estimated = FALSE)
summary(g)$n_parameters


# Section 2.3
g <- gips(S, n, perm = "(12345)", was_mean_estimated = FALSE)
summary(g)$n0

g <- gips(S, n, perm = "()", was_mean_estimated = FALSE)
summary(g)$n0

S_projected <- project_matrix(S, "(12345)")


# Section 2.4
g <- gips(S, n, perm = "(12345)", was_mean_estimated = FALSE)
exp(gips::log_posteriori_of_gips(g))

g2 <- gips(S, n, perm = "(123)", was_mean_estimated = FALSE)
exp(gips::log_posteriori_of_gips(g2))

compare_posteriories_of_perms(g, "(123)")


# Section 2.5
g <- gips(S, n, was_mean_estimated = FALSE)

set.seed(2022)
g_MAP_MH_25 <- find_MAP(g, max_iter = 25, optimizer = "MH") # message is anticipated
g_MAP_MH_25

g_MAP_BF <- find_MAP(g, optimizer = "BF")
g_MAP_BF

compare_posteriories_of_perms(g_MAP_BF, g_MAP_MH_25)

g_MAP_BF_with_probs <- find_MAP(g,
  optimizer = "BF",
  save_all_perms = TRUE, return_probabilities = TRUE
)
head(get_probabilities_from_gips(g_MAP_BF_with_probs), 10)

g_MAP_MH_20000 <- find_MAP(g,
  optimizer = "MH", max_iter = 20000,
  save_all_perms = TRUE, return_probabilities = TRUE
)
head(get_probabilities_from_gips(g_MAP_MH_20000), 10)
