# Preprocess data for topic diversity
syn_list <- lapply(sp_list, function(x) {
  x %>%
    ungroup() %>%
    mutate(index = grepl(x = sp_name, ".", fixed = T)) %>%
    filter(index == F) %>%
    mutate(
      name = str_squish(sp_name) %>% tolower(),
      sp_name = str_squish(sp_name) %>% tolower()
    ) %>%
    separate(name, into = c("a", "b"), sep = " ") %>%
    mutate(a = substr(x = a, 1, 1) %>% paste0(., ".")) %>%
    unite(abbrev, a:b, sep = " ")
})
names(syn_list) <- names(sp_list)


# Articles are assigned to their most probable topic
mk_mob_list <- lapply(names(final_models), function(x) {
  df <- make.dt(final_models[[x]], to_stm[[x]][["meta"]]) %>%
    select(docnum, redlistCategory, sp_name, starts_with("Topic")) %>%
    pivot_longer(starts_with("Topic"), names_to = "topic", values_to = "gamma") %>%
    group_by(docnum, redlistCategory, sp_name) %>%
    slice_max(order_by = gamma) %>%
    mutate(gamma = 1) %>%
    pivot_wider(names_from = topic, values_from = gamma, values_fill = 0) %>%
    ungroup() %>%
    rename(IUCN = redlistCategory) %>%
    mutate(IUCN = fct_relevel(IUCN, c("Regionally extinct", "Critically endangered", "Endangered", "Vulnerable", "Near threatened"))) %>%
    unnest_tokens(name, sp_name, token = "regex", pattern = ",") %>%
    mutate(name = mgsub(
      text.var = name,
      pattern = syn_list[[x]]$abbrev,
      replacement = syn_list[[x]]$sp_name
    )) %>%
    group_by(docnum, name, IUCN) %>%
    summarise(across(starts_with("Topic"), mean)) %>%
    ungroup() %>%
    group_by(name, IUCN) %>%
    summarise(across(starts_with("Topic"), sum)) %>%
    ungroup()

  occ <- select(df, starts_with("Topic"))

  env <- select(df, name, IUCN) %>% mutate(IUCN = str_to_sentence(IUCN))

  res <- make_mob_in(occ, env)

  return(res)
})

names(mk_mob_list) <- names(final_models)

# Get diversity statistics
mob_stats <- lapply(mk_mob_list, function(x) {
  res <- get_mob_stats(x,
    group_var = "IUCN", ref_level = "Near threatened",
    index = c("S_n", "S_PIE", "pct_rare"),
    effort_samples = 10
  )
})

names(mob_stats) <- names(mk_mob_list)

# Plots
lapply(names(mk_mob_list), function(x) {
  w <- 18
  h <- 5.25
  p <- 6
  x_title_r <- ifelse(x == "AMPHIBIA", "Samples", "")
  x_title_a <- ifelse(x == "AMPHIBIA", "Rank", "")

  jpeg(filename = paste0(x, "_raref.jpeg"), width = w / 2, height = h, units = "cm", res = 300, pointsize = p)
  par(mfrow = c(1, 1))
  plot_rarefaction2(mk_mob_list[[x]],
    group_var = "IUCN", ref_level = "Near threatened",
    method = "IBR", xtitle = x_title_r
  )
  dev.off()

  jpeg(filename = paste0(x, "_rad.jpeg"), width = w / 2, height = h, units = "cm", res = 300, pointsize = p)
  par(mfrow = c(1, 1))
  plot_abu2(mk_mob_list[[x]], group_var = "IUCN", ref_level = "Near threatened", type = "rad", pooled = T, xtitle = x_title_a)
  dev.off()

  jpeg(filename = paste0(x, "_S_n.jpeg"), width = w, height = h, units = "cm", res = 300, pointsize = p)
  par(mfrow = c(1, 1))
  plot_mob(mob_stats[[x]], "S_n")
  dev.off()

  jpeg(filename = paste0(x, "_S_PIE.jpeg"), width = w, height = h, units = "cm", res = 300, pointsize = p)
  par(mfrow = c(1, 1))
  plot(mob_stats[[x]], "S_PIE")
  dev.off()

  jpeg(filename = paste0(x, "_pct_rare.jpeg"), width = w, height = h, units = "cm", res = 300, pointsize = p)
  par(mfrow = c(1, 1))
  plot(mob_stats[[x]], "pct_rare")
  dev.off()
})
