
preproc_compas <- function() {
  
  expit <- function(x) exp(x) / (1 + exp(x))
  
  # COMPAS example
  sfm <- SFM_proj("compas")
  
  # pre-process COMPAS data
  data <- compas
  data$race <- as.integer(data$race == "White")
  data$sex <- as.integer(data$sex == "Male")
  data$c_charge_degree <- as.integer(data$c_charge_degree == "F")
  
  compas_path <- file.path("~/fairness/sp-to-pp", "compas-scores-two-years.csv")
  
  data$decile_score <- read.csv(compas_path)$decile_score
  
  data <- as.data.table(data)
  
  # create simple predictions
  res <- setorderv(data[, mean(two_year_recid), by = "decile_score"], "decile_score")
  dec_probs <- res$V1
  
  p_y <- expit(glm(two_year_recid ~ . - decile_score, data = data, 
                   family = "binomial")$fitted.values)
  
  np_shat <- rep(0, nrow(data))
  for (i in 1:10) {
    
    dec_idx <- which(data$decile_score == i)
    
    if (i == 1) {
      
      dec_range <- c(0, 1/2 * (dec_probs[i] + dec_probs[i+1])) 
    } else if (i == 10) {
      dec_range <- c(1 / 2 * (dec_probs[i-1] + dec_probs[i]), 1)
    } else dec_range <- c(1 / 2 * (dec_probs[i-1] + dec_probs[i]), 
                          1/2 * (dec_probs[i] + dec_probs[i+1]))
    
    unif_vals <- runif(length(dec_idx), dec_range[1], dec_range[2])
    
    np_shat[dec_idx] <- unif_vals[order(p_y[dec_idx])]
  }
  
  data$np_shat <- np_shat
  data$decile_score <- NULL
  data <- as.data.frame(data)
  
  list(data, sfm)
}

