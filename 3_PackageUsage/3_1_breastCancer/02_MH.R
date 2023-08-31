# 1 hour 25 minutes on AMD EPYC 7413 Processor (single core, because multithreading would not help here)
# 2 hours 50 minutes on MacBook Air 2017

DATADIR <- file.path(".", "3_PackageUsage", "3_1_breastCancer", "data")
load(file.path(DATADIR, "data2.rda"))
S <- cov(data2)

library(gips)

n <- dim(data2)[1] # 58
p <- dim(data2)[2] # 150
g <- gips(S, n, was_mean_estimated = TRUE, D_matrix = diag(1, p))

set.seed(2022)
g_MAP <- find_MAP(g, max_iter = 150000, optimizer = "MH")

my_sum <- summary(g_MAP)
my_sum$whole_optimization_time # 1 h 25 min

save(g_MAP, file = file.path(DATADIR, "g_MAP.rda"))
