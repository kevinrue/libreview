plot_umap <- function(pca_out, date_annotations) {
  umap.config <- umap.defaults
  umap.config$n_neighbors <- 5
  umap.config$n_epochs <- 1E3
  umap_out <- umap(pca_out@scores)
  plot_data <- umap_out$layout %>% 
    as.data.frame() %>% 
    rownames_to_column("date") %>% 
    mutate(date = format(ymd(date), "%d-%m-%Y")) %>%
    left_join(date_annotations, by = "date")
  ggplot(plot_data, aes(V1, V2, colour = type)) +
    geom_point(size = 3) +
    theme_minimal()
}
