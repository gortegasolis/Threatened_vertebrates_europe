# Check best K
plot_indicators <- function(data) {
  data %>%
    mutate(across(.cols = 2:7, function(x) {
      scales::rescale(as.numeric(x), to = c(0, 1))
    })) %>%
    select(!em.its) %>%
    pivot_longer(., 2:7) %>%
    filter(name %in% c("exclus", "heldout", "semcoh")) %>%
    group_by(K) %>%
    mutate(avg = mean(value)) %>%
    ggplot(aes(
      x = as.numeric(K),
      y = value,
      colour = name,
      linetype = name
    )) +
    geom_point() +
    stat_summary(geom = "line", fun = "mean") +
    geom_smooth(aes(
      x = as.numeric(K),
      y = avg
    ))
}

best_k <- readRDS("best_k.rds")

plot_indicators(best_k[["AMPHIBIA"]]) + labs(title = "Amphibia")
plot_indicators(best_k[["REPTILIA"]]) + labs(title = "Reptilia")
plot_indicators(best_k[["AVES"]]) + labs(title = "Aves")
plot_indicators(best_k[["MAMMALIA"]]) + labs(title = "Mammalia")
