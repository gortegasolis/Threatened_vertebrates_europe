# Search the best K (number of topics)
pacman::p_load(stm, tidyverse, parallel)

sink("sink.txt")
best_k <- lapply(to_stm, function(x) {
  res <- lapply(rep(list(x), 5), function(y) {

    temp <- searchK(y[["documents"]], y[["vocab"]], cores = 20, K = seq(10, 100, 10))
    return(temp[["results"]])
  }) %>% data.table::rbindlist(.)
  return(res)
})
sink()

saveRDS(best_k, "best_k.rds")
