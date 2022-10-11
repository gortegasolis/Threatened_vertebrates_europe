# Search species in titles and abstracts
art_per_class <- list()
for (x in names(sp_list)) {
  art_per_class[[x]] <- art_df %>%
    select(my_ID, TI, AB, DE) %>%
    unite("stm_text", TI:DE, sep = " ") %>%
    mutate(stm_text = tolower(stm_text) %>% str_replace_all(., "-", " "))

  for (y in sp_list[[x]]$sp_name) {
    str_match(
      string = art_per_class[[x]]$stm_text,
      pattern = paste0("\\b", tolower(y), "\\b")
    ) -> art_per_class[[x]][[y]]
  }
  art_per_class[[x]] <- art_per_class[[x]] %>%
    pivot_longer(!c(my_ID, stm_text), names_to = "sp_name") %>%
    na.omit() %>%
    select(!value) %>%
    filter(!sp_name %in% c("P. auritus")) # Exclude coincidences with other classes

  art_per_class[[x]] <- left_join(art_per_class[[x]], sp_list[[x]])

  art_per_class[[x]] <-
    art_per_class[[x]] %>%
    select(!className) %>%
    unique() %>%
    group_by(my_ID, stm_text, redlistCategory) %>%
    summarise(
      sp_name =
        paste(sp_name, collapse = ",")
    )
}

# Save backup to disk
saveRDS(art_per_class, "art_per_class.rds")

# Count species with articles
lapply(art_per_class, function(x) {
  x %>%
    filter(!grepl("\\.", sp_name)) %>%
    unique() %>%
    NROW()
})
