# Load bibliographic data
list.files("Redlist_europe/", pattern = "*.bib", full.names = T) %>%
  mclapply(., function(x) {
    M <- convert2df(file = x, dbsource = "isi", format = "bibtex")
    return(M)
  }, mc.cores = 30) %>%
  data.table::rbindlist(., fill = T) %>%
  unique() -> art_df

# Create ID
art_df$my_ID <- paste0("A", row.names(art_df))
names(art_df)
art_df <- relocate(art_df, my_ID, .before = AU)

art_df$auto_ID <- paste0(art_df$web.of.science.categories., ";", art_df$ID)

# Check auto_ID
select(art_df, web.of.science.categories., ID, auto_ID) %>%
  head() %>%
  View()
