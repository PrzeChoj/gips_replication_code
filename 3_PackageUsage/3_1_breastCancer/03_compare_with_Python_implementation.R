load(file.path(".", "3_PackageUsage", "3_1_breastCancer", "data", "data2.rda"))
S <- cov(data2)
p <- dim(S)[1]

library(gips)

load(file.path(".", "3_PackageUsage", "3_1_breastCancer", "data", "g_MAP.rda"))
my_sum <- summary(g_MAP)

table_comparison <- data.frame(matrix(numeric(4 * 2), nrow = 4, ncol = 2))
colnames(table_comparison) <- c("gips", "Python")
rownames(table_comparison) <- c("n0", "n_parameters", "BIC", "AIC")


table_comparison[1, 1] <- my_sum$n0 # 25
table_comparison[2, 1] <- my_sum$n_parameters # 611
table_comparison[3, 1] <- my_sum$BIC # 8741.694
table_comparison[4, 1] <- my_sum$AIC # 7482.764

my_sum$acceptance_rate # 0.195 %
my_sum$whole_optimization_time # 1 hour 25 minutes

plot(g_MAP, type = "both", logarithmic_x = TRUE)
# We see the search did plateau


# perm found by Graczyk et al. (the same algorithm, same parameters, code in Python):
string_python <- "(0, 1, 138, 148, 60, 51, 7, 144)(2, 10, 8, 88, 5, 101, 119, 3)(4, 46, 89)(6, 12, 137, 90, 116, 141, 142, 71, 145, 49, 135, 21, 56, 86, 123, 113, 83, 29)(9, 98, 38, 20, 100, 25, 36, 72)(11, 76, 99, 132, 121)(13, 18, 75, 146)(14, 70, 126, 109)(15, 91, 82, 33, 139, 26, 48, 136)(16, 97, 68)(17, 64, 133, 87, 106, 74, 107, 105, 81, 108, 122, 67)(19, 50, 134, 104, 37, 95, 24, 44)(22, 110, 23, 41, 66, 42, 130, 111)(30, 57, 65, 93, 80)(31, 32)(34, 92, 63, 85, 127, 147, 131, 102, 59, 149, 143, 128, 117, 69, 96, 120)(35, 84, 140)(43, 55, 118, 125, 103, 77, 78, 47)(45, 129, 114, 73, 115, 58, 112, 124, 94)"
string_new <- stringr::str_replace_all(string_python, pattern = "\\d+", function(number_str) {
  return(as.character(1 + as.numeric(number_str)))
})
perm_python <- gips_perm(string_new, 150)
g_python <- gips(S, 58,
  was_mean_estimated = TRUE,
  perm = perm_python,
  D_matrix = diag(1, p)
)
abline(log_posteriori_of_gips(g_python), 0, col = "green")
# We see our MH was on the height of the python implementation around 10000th iteration

python_sum <- summary(g_python)
table_comparison[1, 2] <- python_sum$n0 # 30
table_comparison[2, 2] <- python_sum$n_parameters # 844
table_comparison[3, 2] <- python_sum$BIC # 9380.918
table_comparison[4, 2] <- python_sum$AIC # 7641.904

table_comparison
#                  gips   Python
# n0             25.000   30.000
# n_parameters  611.000  844.000
# BIC          8741.694 9380.918
# AIC          7482.764 7641.904

compare_posteriories_of_perms(g_MAP, g_python, print_output = FALSE)
# 1.60518*10^29 times ours more likely

save(g_python, file = file.path(".", "3_PackageUsage", "3_1_breastCancer", "data", "g_python.rda"))
