# Bootstrap each network
tic()
bnet_list <- sapply(names(net_list), function(x) {
  bnet <- bootnet(net_list[[x]],
    nCores = 30, nBoots = 100,
    statistics = c("edge", "strength")
  )
}, USE.NAMES = T, simplify = F)
toc() -> time_bnet_list

saveRDS(bnet_list, "bnet_list.rds")
rm(bnet_list)

# Check network stability with case dropping bootstrap
net_stab <- lapply(net_list, function(x) {
  c_net1 <- bootnet(
    x,
    statistics = c("edge", "strength", "closeness", "betweenness"),
    nBoots = 2000,
    nCores = 30,
    type = "case",
    caseN = 20
  )

  res <- list()
  res[["corstability"]] <- corStability(c_net1)
  res[["edge"]] <- plot(c_net1, statistics = c("edge"))
  res[["str_clos_bet"]] <- plot(c_net1, statistics = c("strength", "closeness", "betweenness"))

  return(res)
})

lapply(net_stab, function(x) {
  print(x$corstability)
  readline("press any key")
  plot(x$edge)
  readline("press any key")
  plot(x$str_clos_bet)
  readline("press any key")
})

saveRDS(net_stab, "net_stab.rds")
rm(net_stab)
