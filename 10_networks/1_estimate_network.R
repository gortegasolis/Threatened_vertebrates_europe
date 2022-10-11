# Source libraries
source("0_load_install_libraries.R")

# Import stm models and data
final_models <- readRDS("final_models.rds")
to_stm <- readRDS("to_stm.rds")

# Import labels and species list
labels_list <- readRDS("labels_list.rds")
syn_list <- readRDS("syn_list.rds")

# Get dataframes of results
df_list <- sapply(names(final_models), function(x) {
  df <- stm::make.dt(final_models[[x]],
    meta = to_stm[[x]][["meta"]][, c(1:2, 4:5)]
  ) %>%
    relocate(starts_with("Topic"), .after = sp_name) %>%
    pivot_longer(starts_with("Topic")) %>%
    mutate(name = str_remove(name, "Topic") %>% as.numeric()) %>%
    left_join(., labels_list[[x]], by = c("name" = "topic")) %>%
    rownames_to_column("orig_row") %>%
    unnest_tokens(sp, sp_name, token = "regex", pattern = ",") %>%
    mutate(sp = mgsub(
      text.var = sp,
      pattern = syn_list[[x]]$abbrev,
      replacement = syn_list[[x]]$sp_name
    )) %>%
    group_by(orig_row, docnum, my_ID, AB, redlistCategory, name, value, label, sp) %>%
    summarise() %>%
    ungroup() %>%
    mutate(sp = str_to_sentence(sp)) %>%
    select(-orig_row, -name, -docnum) %>%
    mutate(value = ifelse(value < 0.05, 0, value)) %>%
    # group_by(my_ID, sp) %>%
    # slice_max(order_by = value) %>%
    # mutate(value = 1) %>%
    # ungroup() %>%
    pivot_wider(names_from = label, values_from = value, values_fill = 0) %>%
    group_by(sp, redlistCategory) %>%
    add_tally(name = "n_doc") %>%
    ungroup() %>%
    relocate(n_doc, .after = sp) # %>%
  # select(-my_ID,-AB) %>%
  # group_by(sp, redlistCategory, n_doc) %>%
  # summarise_all(sum) %>%
  # ungroup()

  return(df)
}, USE.NAMES = T, simplify = F)

# Create futures
registerDoFuture()
plan(multisession)

# Estimate network per model
tic()
net_list <- sapply(names(df_list), function(x) {
  print(x)
  net <- df_list[[x]] %>%
    select(6:last_col()) %>%
    estimateNetwork(
      .,
      default = "ggmModSelect",
      corMethod = "npn",
      nCores = 30,
      tuning = 0.5,
      stepwise = T,
      # threshold = T,
      signed = T
    )
  return(net)
}, USE.NAMES = T, simplify = F)
toc() -> time_net_list

saveRDS(net_list, "net_list.rds")

# # Check plots and data
lapply(net_list, function(x) {
  plot <- plot(x)
  readline()
})
#
# lapply(names(net_list), function(x){
#   View(net_list[[x]]$graph)
#   View(df_list[[x]])
#   readline()
# })

rm(final_models)
rm(to_stm)
