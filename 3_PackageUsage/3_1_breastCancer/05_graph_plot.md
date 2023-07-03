Plot of the graph is based the `g_MAP_edge_list.csv` and `g_MAP_vertices_colors.csv` files created in `04_save_graph.R`.

It was produced using the [Cytoscape](https://cytoscape.org/).

When one uses `g_python_edge_list.csv` file (also created in `04_save_graph.R`), then one will reproduce the graph in Fig 6 from Graczyk et al. (2022) <doi:10.1214/22-AOS2174> ("Model selection in the space of Gaussian models invariant by symmetry").

Layout and colors:
1. default-black style
2. Edge-weighted Spring Embedded Layout based on "value" column of `g_MAP_edge_list.csv`. We intended to use the "for_embedding" column, but it turned out to be worse, so we rejected it. This was the basis for out layout, but then we adjusted the placement of the nodes according to out preferences.
3. Colors of vertices were based on "colors" column of `g_MAP_vertices_colors.csv`.
