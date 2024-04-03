
preproc_miiv <- function() {
  
  # load the mortality data
  patient_ids <- id_col(load_concepts("adm_episode", "miiv", 
                                      verbose = FALSE)[adm_episode == 1])
  dat <- load_concepts(c("acu_24", "diag", "age", "sex", "charlson",
                         "lact_24", "pafi_24", "ast_24",
                         "race", "death", "los_icu"), "miiv", 
                       patient_ids = patient_ids,
                       verbose = FALSE)
  dat <- dat[race %in% c("Caucasian", "African American")]
  dat[, c(index_var(dat)) := NULL]
  dat[, pci := los_icu >= 10]
  dat[, pci_or_death := death | pci]
  dat[, c("pci", "los_icu", "death") := NULL]
  
  # remove factors
  dat[, race := (race == "Caucasian")]
  dat[, sex := (race == "Male")]
  diag_mat <- model.matrix(~ . - 1, dat[, "diag"])[, -1]
  dat[, diag := NULL]
  dat <- cbind(dat, diag_mat)
  
  imp_lst <- list(
    age = 65,
    acu_24 = 0,
    charlson = 0,
    lact_24 = 1,
    ast_24 = 20,
    pafi_24 = 500,
    pci_or_death = 0
  )
  
  for (i in seq_len(ncol(dat))) {
    
    var <- names(dat)[i]
    if (any(is.na(dat[[var]])) & !is.null(imp_lst[[var]]))
      dat[is.na(get(var)), c(var) := imp_lst[[var]]]
  }
  
  sfm <- list(
    X = "race", Z = c("age", "sex"),
    W = c("acu_24", colnames(diag_mat), "charlson", "lact_24", "pafi_24", 
          "ast_24"),
    Y = "pci_or_death"
  )
  
  list(dat, sfm)
}
