# Degree against conservation words

final_models <- readRDS("final_models.rds")

plot_con_degree <- sapply(names(df_list), function(x) {
  df1 <- final_models[[x]] %>%
    tidy() %>%
    filter(term %in% c("conservation", "threat", "endanger", "extinct", "conservationist")) %>%
    group_by(topic) %>%
    summarise(beta = sum(beta)) %>%
    left_join(., labels_list[[x]]) %>%
    select(label, beta)
  
  df2 <- b_plots[[x]][["degree"]][["data"]] %>%
    # filter(var == "mean") %>%
    select(id, var, value) %>%
    pivot_wider(names_from = var) %>%
    left_join(df1, ., by = c("label" = "id")) %>%
    mutate(col = ifelse(sample == 0,
                        "Isolated", "Connected"
    ) %>% fct_relevel("Isolated", "Connected"))

  plot <- ggscatter(df2, x = "beta", y = "mean", color = "col") +
    geom_text_repel(aes(label = label), 
                    force = 2, force_pull = 0, nudge_y = -0.015, 
                    max.time = 20, max.iter = 20000) +
    stat_cor(method = "spearman", label.x.npc = 0.805,
             p.digits = 2, r.digits = 1,digits = 2) + 
    scale_y_continuous(limits = c(0,1.12)) +
    xlab("Probability of conservation words") +
    ylab("Bootstrapped degree (mean)") +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}, USE.NAMES = T, simplify = F)

ggpubr::ggarrange(plotlist = rev(plot_con_degree), common.legend = TRUE, legend = "bottom", labels = "AUTO") %>%
  ggsave("con_vs_degree.jpeg", plot = ., units = "cm", width = 36, height = 36)
