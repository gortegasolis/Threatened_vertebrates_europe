# Plot network
p_net <- sapply(names(net_list), function(x) {
  res <- list()

  igraph <- qgraph::qgraph(net_list[[x]][["graph"]], DoNotPlot = T) %>%
    as.igraph(., attributes = TRUE)

  # res[["igraph"]] <- igraph

  graph <- network(net_list[[x]][["graph"]])

  comm_df <- data.frame("community" = comm_list[[x]]) %>%
    group_by(community) %>%
    add_tally() %>%
    ungroup() %>%
    mutate(community = ifelse(n == 1, 100, community))

  graph %v% "Clusters" <- comm_df$community

  graph %v% "Strength" <- igraph::degree(igraph) %>% as.numeric()

  graph %e% "edgecol" <- ifelse(E(igraph)$weight > 0, "steelblue", "red")

  graph %e% "weight" <- E(igraph)$weight %>% scales::rescale(., to = c(0.25, 2.25))

  res[["plot"]] <- ggnet2(graph, "kamadakawai",
    label = F,
    size = "Strength",
    size.min = 1,
    color = "Clusters",
    edge.size = "weight",
    edge.color = "edgecol",
    legend.position = "bottom",
    size.legend = "Degree",
    palette = "Spectral"
  ) +
    geom_nodetext_repel(aes(label = label))

  return(res)
}, USE.NAMES = T, simplify = F)

# Check plots
p_net$AMPHIBIA[["plot"]]
p_net$REPTILIA[["plot"]]
p_net$AVES[["plot"]]
p_net$MAMMALIA[["plot"]]

# Plot bootstrapped values for confidence intervals of centrality measures
b_plots <- lapply(bnet_list, function(x) {
  res <- list()
  # Plot bootstrapped degree (node strength) CIs
  plot(
    x,
    c("strength"),
    plot = "area",
    order = "sample",
    labels = T,
    legend = F,
    panels = T,
  ) +
    # labs(tag = "C") +
    theme_pubr() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10),
      plot.margin = margin(1, 1, 1, 10),
      plot.tag = element_text(size = 30),
      strip.background = element_blank(),
      strip.text = element_blank()
    ) -> res[["degree"]]

  # Plot bootstrapped edge CIs
  plot(
    x,
    "edge",
    plot = "area",
    order = "sample",
    labels = T,
    legend = F,
    panels = F,
  ) +
    # labs(tag = "B") +
    theme_pubr() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 10),
      plot.tag = element_text(size = 30)
    ) -> res[["edge"]]

  return(res)
})

lapply(b_plots, function(x) {
  plot(x[["degree"]])
  readline("press any key")
  plot(x[["edge"]])
  readline("press any key")
})

# Plot significant differences (alpha = 0.05) of node degree
plot(
  b_net1,
  "strength",
  plot = "difference",
  order = "sample",
  legend = F,
  panels = F,
  labels = T
) +
  theme_pubr() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 60,
      vjust = 1,
      hjust = 1
    )
  )

# Plot significant differences (alpha = 0.05) of edges:
plot(
  b_net1,
  "edge",
  plot = "difference",
  order = "sample",
  legend = T,
  panels = F,
  labels = F,
  onlyNonZero = T
) +
  theme_pubr() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    )
  ) -> edge_sig_diff



# Final network plots
final_net_plots <- sapply(names(p_net), function(x) {
  plot <- egg::ggarrange(
    p_net[[x]][["plot"]],
    b_plots[[x]][["degree"]],
    ncol = 2,
    widths = c(2, 1),
    labels = c("A", "B"),
    label.args = list(gp = grid::gpar(font = 2, fontsize = 24), x = unit(1, "line"), hjust = 0)
  )
  ggsave(
    plot = plot, filename = paste0("final_net_", x, ".jpeg"),
    units = "cm",
    width = 36,
    height = 24,
    dpi = 300
  )
}, USE.NAMES = T, simplify = F)

lapply(final_net_plots, function(x) {
  plot(x)
  readline("press any key")
})
