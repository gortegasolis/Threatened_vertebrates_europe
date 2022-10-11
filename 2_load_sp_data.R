# Load species data
sp_list <- read_csv("Redlist_europe/taxonomy.csv") %>%
  select(scientificName, className) %>%
  left_join(., select(
    read_csv("Redlist_europe/synonyms.csv"),
    scientificName, Synonym
  )) %>%
  left_join(., select(
    read_csv("Redlist_europe/assessments.csv"),
    scientificName, redlistCategory
  )) %>%
  filter(redlistCategory %in% c("Near Threatened", "Vulnerable", "Endangered", "Critically Endangered", "Regionally Extinct")) %>%
  pivot_longer(!c(className, redlistCategory), values_to = "sp_name") %>%
  na.omit()

table(sp_list$className)

sp_list$abb <- paste(str_extract(sp_list$sp_name, "\\w"),
  word(sp_list$sp_name, -1),
  sep = ". "
)

sp_list <- sp_list %>%
  select(!name) %>%
  pivot_longer(!c(className, redlistCategory), values_to = "sp_name") %>%
  select(className, sp_name, redlistCategory) %>%
  group_by(sp_name) %>%
  filter(n() == 1)

# Get common names
require(taxize)

comm_list <- lapply(sp_list$sp_name, function(x) {
  res <- sci2comm(x)
  res2 <- str_split(res, " ")
  return(res2)
}) %>% unlist(., recursive = T)

comm_list <- comm_list %>% c(., paste0(comm_list, "s"))

# Split splist per class
sp_list <- split(sp_list, sp_list$className)

# Count species
lapply(sp_list, function(x) {
  x %>%
    filter(!grepl("\\.", sp_name)) %>%
    unique() %>%
    NROW()
})
