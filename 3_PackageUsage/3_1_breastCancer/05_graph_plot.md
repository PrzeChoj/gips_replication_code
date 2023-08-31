Plot of the graph (Figure 7) is based the `g_MAP_edge_list.csv` and `g_MAP_vertices_colors.csv` files created in `04_save_graph.R`.

It was produced with the [Cytoscape](https://cytoscape.org/).

If one uses `g_python_edge_list.csv` file (also created in `04_save_graph.R`), then one would reproduce the graph in Figure 6 from Graczyk et al. (2022) <doi:10.1214/22-AOS2174> ("Model selection in the space of Gaussian models invariant by symmetry").

Layout and colors:
1. default-black style
2. Edge-weighted Spring Embedded Layout based on "value" column of `g_MAP_edge_list.csv`. We intended to use the "for_embedding" column, but it turned out to be worse, so we rejected it. This was the basis for out layout, but then we adjusted the placement of the nodes according to out preferences.
3. Colors of vertices were based on "colors" column of `g_MAP_vertices_colors.csv`.
