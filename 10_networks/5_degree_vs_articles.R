# Degree against number of articles

plot_art_degree <- sapply(names(df_list), function(x) {
  df1 <- df_list[[x]] %>%
    select(my_ID, redlistCategory, 6:last_col()) %>%
    pivot_longer(3:last_col(), names_to = "label") %>%
    group_by(my_ID) %>%
    slice_max(order_by = value) %>%
    mutate(value = 1) %>%
    ungroup() %>%
    group_by(label) %>%
    summarise("articles" = n()) %>%
    ungroup() %>%
    unique()

  df2 <- b_plots[[x]][["degree"]][["data"]] %>%
    # filter(var == "mean") %>%
    select(id, var, value) %>%
    pivot_wider(names_from = var) %>%
    left_join(df1, ., by = c("label" = "id")) %>%
    mutate(col = ifelse(sample == 0,
      "Isolated", "Connected"
    ) %>% fct_relevel("Isolated", "Connected"))

  plot <- ggscatter(df2, x = "articles", y = "mean", color = "col") +
    geom_text_repel(aes(label = label), 
                    force = 2, force_pull = 0, nudge_y = -0.015, 
                    max.time = 20, max.iter = 20000) +
    stat_cor(method = "spearman", label.x.npc = 0.805,
             p.digits = 2, r.digits = 1,digits = 2) + 
    scale_y_continuous(limits = c(0,1.12)) +
    xlab("N° articles") +
    ylab("Bootstrapped degree (mean)") +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}, USE.NAMES = T, simplify = F)

ggpubr::ggarrange(plotlist = rev(plot_art_degree), common.legend = TRUE, legend = "bottom", labels = "AUTO") %>%
  ggsave("art_vs_degree.jpeg", plot = ., units = "cm", width = 36, height = 30)
