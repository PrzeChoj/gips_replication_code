# 5 minutes on AMD EPYC 7413 Processor (single core, because multithreading would not help here)

library(gips)
library(igraph)
library(gRim)
library(RBGL)

DATADIR <- file.path(".", "3_PackageUsage", "3_1_breastCancer", "data")

load(file.path(DATADIR, "data2.rda"))
S <- cov(data2)
n <- nrow(data2)
p <- ncol(data2)

library(gips)

load(file.path(DATADIR, "g_MAP.rda"))
load(file.path(DATADIR, "g_python.rda"))



# Get graph and BIC
graph_from_g_MAP <- function(gips_object, my_quantile) {
  S <- attr(gips_object, "S")
  S_MAP <- project_matrix(S, gips_object)
  K <- solve(S_MAP)
  
  # We know there is gRbase::cov2pcor(), but there was sth wrong with it on the cluster we are using...
  PC_MAP <- -cov2cor(K)
  diag(PC_MAP) <- -diag(PC_MAP)
  
  cutoff_val <- quantile(abs(PC_MAP), my_quantile)

  adj_mat <- (abs(PC_MAP) > cutoff_val) * 1
  diag(adj_mat) <- 0
  
  my_graph <- graph_from_adjacency_matrix(adj_mat, mode = "undirected")
  
  # BIC:
  maxCliques_G <- maxClique(as_graphnel(my_graph))$maxCliques
  MLE_MAP <- ggmfit(S_MAP, n = n, maxCliques_G)
  kG <- length(unique(c(S_MAP * adj_mat))) - 1
  BICG <- kG * log(n) - 2 * (-((n - 1) * p / 2) * log(2 * pi) + ((n - 1) / 2) * log(MLE_MAP$detK) - 0.5 * (n - 1) * p)
  print(paste0("BIC: ", BICG))
  
  # for the plot:
  K_masked <- K * adj_mat
  my_graph_weighted <- graph_from_adjacency_matrix(K_masked, mode = "undirected", weighted = TRUE)
  my_graph_weighted <- simplify(my_graph_weighted)
  
  # edge list:
  my_edge_list <- get.edgelist(my_graph_weighted)
  my_edges_data_frame <- data.frame(my_edge_list)
  colnames(my_edges_data_frame) <- c("from", "to")
  my_edges_data_frame$value <- mapply(function(row, col) K_masked[row, col], my_edges_data_frame$from, my_edges_data_frame$to)
  my_edges_data_frame$value <- round(my_edges_data_frame$value, 3)
  my_edges_data_frame$for_embedding <- abs(my_edges_data_frame$value)
  big_edge <- max(my_edges_data_frame$for_embedding)

  # vertices have the same colors iff vertices have the same variance:
  my_perm <- gips_object[[1]]
  list_of_cycles <- unclass(my_perm)
  num_of_cycles <- length(list_of_cycles)

  my_vertices_data_frame <- data.frame(colnames(attr(gips_object, "S")))
  colnames(my_vertices_data_frame) <- "name"
  my_vertices_data_frame$colors <- numeric(150)

  for (i in 1:num_of_cycles) {
    # vertices
    my_vertices_data_frame[list_of_cycles[[i]], "colors"] <- i

    # edges
    potential_from_edge <- (my_edges_data_frame$from %in% my_vertices_data_frame[list_of_cycles[[i]], "name"])
    potential_to_edge <- (my_edges_data_frame$to %in% my_vertices_data_frame[list_of_cycles[[i]], "name"])
    edge_is_in_cluster <- potential_from_edge & potential_to_edge

    my_edges_data_frame[edge_is_in_cluster, "for_embedding"] <- 10 * big_edge
  }


  invisible(list(BICG, my_graph_weighted, my_edges_data_frame, my_vertices_data_frame))
}


# python
BIC(g_python) # This is the full model; BIC: 9381
graph_python <- graph_from_g_MAP(g_python, 0.9112) # Graczyk et al said, they used alpha = cutoff_val = 0.15 = quantile(0.9112)
graph_python[[1]] # This is the smaller graph model; BIC: 9174
# 9174 < 9381, so the smaller is better

igraph_python <- graph_python[[2]]
save(igraph_python, file = file.path(DATADIR, "igraph_python.rda"))
write.csv(graph_python[[3]], file.path(DATADIR, "g_python_edge_list.csv"), row.names = FALSE, quote = FALSE)
write.csv(graph_python[[4]], file.path(DATADIR, "g_python_vertices_colors.csv"), row.names = FALSE, quote = FALSE)


# gips
BIC(g_MAP) # This is the full model; BIC: 8644
graph_MAP <- graph_from_g_MAP(g_MAP, 0.7) # 0.7 minimizes the BIC
graph_MAP[[1]] # This is smaller graph model; BIC: 7807
# 7807 < 8644, so smaller is better

igraph_MAP <- graph_MAP[[2]]
save(igraph_MAP, file = file.path(DATADIR, "igraph_MAP.rda"))
write.csv(graph_MAP[[3]], file.path(DATADIR, "g_MAP_edge_list.csv"), row.names = FALSE, quote = FALSE)
write.csv(graph_MAP[[4]], file.path(DATADIR, "g_MAP_vertices_colors.csv"), row.names = FALSE, quote = FALSE)
