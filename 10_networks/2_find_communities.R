# Find communities
comm_list <- lapply(net_list, function(x) {
  g <- qgraph::qgraph(x[["graph"]], DoNotPlot = T) %>%
    as.igraph(., attributes = TRUE)

  E(g)$weight <- E(g)$weight %>% scales::rescale(., to = c(1, 100))

  res <- cluster_walktrap(g, weights = NULL)$membership

  return(res)
})

# Check plots and data
lapply(names(net_list), function(x) {
  plot <-
    plot(
      net_list[[x]],
      groups = comm_list[[x]]$membership,
      palette = "rainbow",
      layout = "spring"
    )
  readline()
})
