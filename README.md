# gips_replication_code

This repository contains replication codes for the article about the `gips` R package.

The article is available [HERE on arXiv](https://arxiv.org/abs/2307.00790).

The code is design to run under `gips` 1.2.1.

To run it, install the following packages (in this order):
```
install.packages("gips")

install.packages(c("ggplot2", "HSAUR2", "DAAG", "dplyr", "tidyr", "BiocManager",
                   "igraph", "gRim", "magrittr", "huge", "mvtnorm"))
BiocManager::install(c("GEOquery", "RBGL"))
install.packages("rags2ridges")
```

All folders are independent and produce plots to the `plots` folder. Inside the `3_(1|2|3)_*` folders, the scripts are numbered in the order of running. If the code produces some output files, they are saved in this repository in the appropriate `data` folders. Time-consuming scripts have information in the first line on how long it took us to run.
