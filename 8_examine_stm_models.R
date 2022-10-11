# Export candidate models
doc_per_topic <- sapply(names(cand_stm), function(x) {
  if (x == "AMPHIBIA") {
    num <- 1:10
  }
  if (x == "REPTILIA") {
    num <- 1:10
  }
  if (x == "AVES") {
    num <- 1:10
  }
  if (x == "MAMMALIA") {
    num <- 1:10
  }

  fin <- lapply(cand_stm[[x]][num], function(y) {
    res <- make.dt(y, meta = to_stm[[x]][["meta"]]) %>%
      mutate(across(starts_with("Topic"), round, 2)) %>%
      select(my_ID, sp_name, stm_text, starts_with("Topic")) %>%
      mutate(rowmax = apply(select(., starts_with("Topic")), 1, FUN = max)) %>%
      filter(rowmax > 0.5)

    colnames(res) <- labelTopics(y, n = 10)$frex %>%
      as.data.frame() %>%
      unite(col = "words") %>%
      pull("words") %>%
      c(
        "my_ID", "matched_species",
        "stm_text", ., "rowmax"
      )

    res <-
      right_join(
        select(art_df, my_ID, AU, TI, DE, AB, SO, DT, VL, PP) %>%
          unique(),
        res
      )

    return(res)
  })
}, USE.NAMES = T, simplify = F)

# Export to xlsx
for (x in names(doc_per_topic)) {
  if (x == "AMPHIBIA") {
    num <- 1:10
  }
  if (x == "REPTILIA") {
    num <- 1:10
  }
  if (x == "AVES") {
    num <- 1:10
  }
  if (x == "MAMMALIA") {
    num <- 1:10
  }

  try(filename <- paste0("stm_res_", x, ".xlsx"))

  system(paste0("mv -f ", filename, " bck_", filename))
  Sys.sleep(5)

  openxlsx::write.xlsx(
    doc_per_topic[[x]][num],
    filename,
    asTable = T,
    firstRow = T,
    firstActiveCol = 11,
    withFilter = T,
    zoom = 125,
    sheetName = paste0("model_", 1:length(doc_per_topic[[x]][num]))
  )
}
rm(list = c("x", "num", "filename"))
