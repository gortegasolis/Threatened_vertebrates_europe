# Check label words and texts
write_labels <-
  function(model = NULL,
           texts,
           old_labels = NULL,
           n = 10,
           n_texts = 5,
           plot_type = "wordcloud1",
           cex = 0.8,
           t_width = 200) {
    res <- lapply(
      1:model[["settings"]][["dim"]][["K"]],
      function(x) {

        # Print labelTopics function
        labelTopics(model,
          topics = x,
          n = n
        ) %>% print()

        # Get findThoughts object
        findThoughts(model,
          texts,
          topics = x,
          n = n_texts,
          thresh = 0.5
        ) -> plot_texts

        if (is.null(old_labels) == F) {
          if (NROW(old_labels) >= x) {
            title <- filter(old_labels, topic == x)[["label"]][[1]]
          } else {
            title <- "empty"
          }
        }

        # Wordcloud 1
        if (plot_type == "wordcloud1") {
          plot_texts[["docs"]][[paste("Topic", x)]] %>%
            VectorSource() %>%
            Corpus() %>%
            TermDocumentMatrix() %>%
            tidy() -> m
          ggplot(
            data = m,
            aes(label = term, size = count)
          ) +
            geom_text_wordcloud()
        }

        # Wordcloud 2
        if (plot_type == "wordcloud2") {
          plot_texts[["docs"]][[paste("Topic", x)]] %>%
            lapply(., function(d) {
              VectorSource(d) %>%
                Corpus() %>%
                TermDocumentMatrix() %>%
                tidy() -> m
              ggplot(
                data = m,
                aes(label = term, size = count)
              ) +
                geom_text_wordcloud() -> p
              return(p)
            }) -> plot_list

          do.call("grid.arrange", c(plot_list, ncol = 2, top = title))
        }

        # Plot quote
        if (plot_type == "plotQuote") {
          par(mar = c(0.1, 0.1, 1, 0.1))

          if (is.null(old_labels) == F) {
            plot(plot_texts,
              main = paste("Old label:", title),
              width = t_width,
              text.cex = cex
            )
          } else {
            plot(plot_texts, width = t_width, text.cex = cex)
          }
        }

        # Get user input
        label <- readline()

        # Keep old label
        if (nchar(label) == 0) {
          label <- title
        }

        # Get label data frame
        paste0(label, "\n") %>%
          crayon::bgRed() %>%
          cat()
        res <-
          data.frame(
            "topic" = x,
            "label" = label %>% str_to_sentence()
          )
      }
    ) %>% data.table::rbindlist() # Rbind labels
    # Show labels
    print(res)

    # Return final result
    return(res)
  }


# labels_list <- list()

labels_list[["AMPHIBIA"]] <-
  write_labels(
    model = cand_stm$AMPHIBIA[[3]],
    texts = to_stm$AMPHIBIA$meta$AB,
    old_labels = labels_list$AMPHIBIA,
    plot_type = "plotQuote",
    t_width = 150,
    cex = 0.7
  )

labels_list[["REPTILIA"]] <-
  write_labels(
    model = cand_stm$REPTILIA[[4]],
    texts = to_stm$REPTILIA$meta$AB,
    old_labels = labels_list$REPTILIA,
    plot_type = "plotQuote",
    t_width = 150,
    cex = 0.7
  )

labels_list[["AVES"]] <-
  write_labels(
    model = cand_stm$AVES[[3]],
    texts = to_stm$AVES$meta$AB,
    old_labels = labels_list$AVES,
    plot_type = "plotQuote",
    t_width = 150,
    cex = 0.7
  )

labels_list[["MAMMALIA"]] <-
  write_labels(
    model = cand_stm$MAMMALIA[[4]],
    texts = to_stm$MAMMALIA$meta$AB,
    old_labels = labels_list$MAMMALIA,
    plot_type = "plotQuote",
    t_width = 150,
    cex = 0.7
  )

lapply(labels_list, print)

lapply(labels_list, NROW)

# labels_list[["AMPHIBIA"]] %>% group_by(label) %>% filter(n()>1)
# labels_list[["REPTILIA"]] %>% group_by(label) %>% filter(n()>1)
# labels_list[["AVES"]] %>% group_by(label) %>% filter(n()>1)
# labels_list[["MAMMALIA"]] %>% group_by(label) %>% filter(n()>1)

# Export final models
final_models <- list()
final_models[["AMPHIBIA"]] <- cand_stm[["AMPHIBIA"]][[3]] # 37 topics
final_models[["REPTILIA"]] <- cand_stm[["REPTILIA"]][[4]] # 37 topics
final_models[["AVES"]] <- cand_stm[["AVES"]][[3]] # 40 topics
final_models[["MAMMALIA"]] <- cand_stm[["MAMMALIA"]][[4]] # 40 topics

names(final_models)

saveRDS(final_models, "final_models.rds")

df_final_mod <- sapply(names(final_models), function(x) {
  res <- make.dt(final_models[[x]], to_stm[[x]]$meta[, c("AB")]) %>%
    pivot_longer(starts_with("Topic")) %>%
    group_by(name) %>%
    slice_max(order_by = value, n = 10) %>%
    filter(value > 0.5) %>%
    mutate(name = gsub(x = name, "Topic", "") %>% as.integer()) %>%
    left_join(., labels_list[[x]], by = c("name" = "topic"))
}, USE.NAMES = T, simplify = F)

openxlsx::write.xlsx(df_final_mod, "df_final_mod.xlsx")
# system(paste0("ln ",getwd(),"/final_models.rds ",getwd(),"/10_networks/final_models.rds"))

# Top topics
top_topic <- sapply(names(final_models), function(x) {
  res <- make.dt(final_models[[x]], to_stm[[x]][["meta"]]) %>%
    select(redlistCategory, starts_with("Topic")) %>%
    pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
    group_by(redlistCategory, topic) %>%
    summarise(gamma = mean(gamma)) %>%
    mutate(topic = gsub(x = topic, pattern = "Topic", "") %>% as.integer()) %>%
    left_join(., labels_list[[x]]) %>%
    mutate(redlistCategory = str_to_sentence(redlistCategory) %>%
      fct_relevel(., c("Regionally extinct", "Critically endangered", "Endangered", "Vulnerable", "Near threatened"))) %>%
    group_by(redlistCategory) %>%
    slice_max(gamma, n = 3) %>%
    ggplot(., aes(x = reorder_within(label, gamma, within = redlistCategory), y = gamma)) +
    geom_col() +
    scale_x_reordered() +
    coord_flip() +
    facet_rep_wrap(~redlistCategory, ncol = 1, scales = "free_y")
}, USE.NAMES = T, simplify = F)

top_topic[["AMPHIBIA"]]
top_topic[["REPTILIA"]]
top_topic[["AVES"]]
top_topic[["MAMMALIA"]]

# Articles per dominant topic
test <- sapply(names(final_models), function(x) {
  df <- make.dt(final_models[[x]]) %>%
    select(docnum, starts_with("Topic")) %>%
    pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
    group_by(docnum) %>%
    slice_max(order_by = gamma) %>%
    left_join(., mutate(labels_list[[x]], topic = paste0("Topic", topic))) %>%
    group_by(label) %>%
    summarise(n())
}, USE.NAMES = T, simplify = F)

df <- make.dt(final_models[[x]], to_stm[[x]][["meta"]]) %>%
  select(docnum, redlistCategory, sp_name, starts_with("Topic")) %>%
  pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
  group_by(docnum, redlistCategory, sp_name) %>%
  slice_max(order_by = gamma)

# Total topic count per conservation status
t_count <- sapply(names(final_models), function(x) {
  res <- make.dt(final_models[[x]], to_stm[[x]][["meta"]]) %>%
    select(docnum, redlistCategory, starts_with("Topic")) %>%
    pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
    group_by(redlistCategory, docnum) %>%
    slice_max(., order_by = gamma) %>%
    ungroup() %>%
    group_by(redlistCategory) %>%
    add_tally(name = "Tot_art") %>%
    ungroup() %>%
    select(redlistCategory, Tot_art, topic) %>%
    mutate(redlistCategory = str_to_sentence(redlistCategory)) %>%
    mutate(redlistCategory = fct_relevel(redlistCategory, c("Regionally extinct", "Critically endangered", "Endangered", "Vulnerable", "Near threatened"))) %>%
    group_by(redlistCategory, Tot_art, topic) %>%
    slice_head(., n = 1) %>%
    ungroup() %>%
    group_by(redlistCategory, Tot_art) %>%
    add_tally(name = "Tot_top") %>%
    summarise(Tot_top = mean(Tot_top))

  plot1 <- ggplot(res, aes(x = redlistCategory, y = Tot_art)) +
    geom_col() +
    ggpubr::theme_pubr() +
    xlab("") +
    ylab("Articles") +
    coord_flip()

  plot2 <- ggplot(res, aes(x = redlistCategory, y = Tot_top)) +
    geom_col() +
    ggpubr::theme_pubr() +
    xlab("") +
    ylab("Topic count") +
    coord_flip()

  egg_plot <- egg::ggarrange(plot1, plot2,
    ncol = 2,
    labels = c("A", "B"),
    label.args = list(gp = grid::gpar(font = 2))
  )
}, USE.NAMES = T, simplify = F)

# Conservation related words
cons_words <- sapply(names(final_models), function(x) {
  res1 <- tidy(final_models[[x]]) %>%
    filter(term %in% c("conservation", "threat", "endanger", "extinct", "conservationist")) %>%
    group_by(topic) %>%
    summarise(beta = sum(beta))
}, USE.NAMES = T, simplify = F)

# Effect of conservation status on topic frequency
iucn_effects <- sapply(names(final_models), function(x) {
  res1 <- tidy(final_models[[x]]) %>%
    filter(term %in% c("conservation", "threat", "endanger", "extinct", "conservationist")) %>%
    group_by(topic) %>%
    summarise(beta = sum(beta))

  set.seed(123456)

  res2 <-
    estimateEffect(final_models[[x]],
      formula = ~redlistCategory,
      metadata = to_stm[[x]][["meta"]],
      nsims = 100
    ) %>%
    get_effects(type = "pointestimate", variable = "redlistCategory") %>%
    mutate(class = str_to_sentence(x)) %>%
    left_join(., labels_list[[x]] %>% mutate(topic = as.factor(topic))) %>%
    left_join(., res1 %>% mutate(topic = as.factor(topic))) %>%
    mutate(value = str_to_sentence(value)) %>%
    mutate(value = fct_relevel(value, c("Regionally extinct", "Critically endangered", "Endangered", "Vulnerable", "Near threatened")))

  plot1 <- ggplot(res2, aes(
    x = reorder(label, beta),
    y = proportion,
    col = value,
    group = value
  )) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
      width = 0.1, size = 1
    ) +
    geom_point(
      size = 3
    ) +
    geom_hline(yintercept = 0, color = "red") +
    scale_y_continuous(breaks = round(seq(min(res2$lower),
      max(res2$upper),
      by = 0.5 * (max(res2$upper) - min(res2$lower))
    ), 2)) +
    facet_grid(~value) +
    coord_flip() +
    ggpubr::theme_pubr() +
    theme(
      panel.spacing.x = unit(6, "mm"),
      strip.text = element_blank(),
      strip.background = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    labs(x = "", y = "Mean topic frequency") +
    ggthemes::scale_colour_colorblind()

  # plot1 <- tag_facet_outside2(plot1, open = "", close = "", tag_fun_top = function(i) c(1:10)[i])

  plot2 <- ggplot(res2, aes(
    x = reorder(label, beta),
    y = beta,
    group = value
  )) +
    stat_summary(geom = "col", fun = mean) +
    coord_flip() +
    ggpubr::theme_pubr() +
    labs(x = "", y = "Tot. frequency of conservation words") +
    theme(axis.text.y = element_blank())

  fin <- egg::ggarrange(plot1,
    plot2,
    ncol = 2, widths = c(3, 1),
    labels = c("A", "B"),
    label.args = list(gp = grid::gpar(font = 2))
  )

  return(fin)
}, USE.NAMES = T, simplify = F)

iucn_effects$AMPHIBIA
iucn_effects$REPTILIA
iucn_effects$AVES
iucn_effects$MAMMALIA

lapply(names(iucn_effects), function(x) {
  ggsave(iucn_effects[[x]],
    filename = paste0("topic_freq_cons_wrd_", x, ".jpg"),
    width = 37, height = 25, units = "cm",
    pointsize = 16
  )
})

# Save and change project
documentSaveAll()
openProject("10_networks/Networks.Rproj")
