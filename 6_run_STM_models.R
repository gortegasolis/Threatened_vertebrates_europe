# Load libraries
source("0_libraries.R")

# Run candidate models
cand_K <- list()
cand_K[["AMPHIBIA"]] <- seq(25, 45, 2)
cand_K[["REPTILIA"]] <- seq(25, 45, 2)
cand_K[["AVES"]] <- seq(45, 70, 2)
cand_K[["MAMMALIA"]] <- seq(45, 70, 2)

# sink("cand_models_output.txt")
cand_stm <- sapply(names(cand_K), function(x) {
  formula_df <-
    to_stm[[x]][["meta"]] %>% select(starts_with("prev_"))
  formula <- select(formula_df, which(!colSums(formula_df, na.rm = TRUE) %in% c(0, 1))) %>%
    names() %>%
    paste(., collapse = "+") %>%
    paste("~redlistCategory+", .) %>%
    as.formula()

  res <- mclapply(cand_K[[x]], function(y) {
    temp <- stm(
      documents = to_stm[[x]][["documents"]],
      vocab = to_stm[[x]][["vocab"]],
      data = to_stm[[x]][["meta"]],
      prevalence = formula,
      K = y,
      verbose = F,
      reportevery = 0
    )
    return(temp)
  }, mc.cores = 25)
  return(res)
}, USE.NAMES = T, simplify = F)
# sink()

saveRDS(cand_stm, "cand_stm.rds")
