
mind_the_gap <- function(X, Z, W, Y, data, x0, x1, thr = 0.5) {
  
  thresh <- function(x, threshold) as.integer(x >= threshold)
  wgh_sum <- function(x, wgh) sum(x * wgh) / sum(wgh)
  
  compute_po <- function(fx = 0, wx = 0, zx = 0, thresh = FALSE, norm = "adapt") {
    
    if (is.na(zx)) zx <- NA
    
    if (wx == 0) wgh <- (1 - px_zw) / (1 - px_z) else px_zw / px_z
    if (zx == 0) wgh <- wgh * (1 - px_zw) else wgh <- wgh * px_z 
    
    if (fx == 0) po_samp <- probx0 else po_samp <- probx1
    if (norm == "adapt") norm_const <- sum(wgh) else norm_const <- length(po_samp)
    list(po_samp = as.integer(po_samp > thr), wgh = wgh, norm_const = norm_const)
  }
  
  diff_po <- function()
  
  if (is.logical(data[[Y]])) data[[Y]] <- as.integer(data[[Y]])
  assertthat::assert_that(all(data[[Y]] %in% c(0, 1)))
  
  # build a predictor (xgboost) for the binary outcome Y
  dtrain <- xgb.DMatrix(data = as.matrix(data[, c(X, Z, W)]), label = data[[Y]])
  
  # use the logistic loss
  params <- list(objective = "binary:logistic", eval_metric = "logloss")
  
  # cross-validation to find optimal nrounds
  cv <- xgb.cv(
    params = params, data = dtrain, nfold = 5, nrounds = 100,
    early_stopping_rounds = 10, verbose = 0
  )
  optimal_nrounds <- cv$best_iteration
  
  # train the model
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = optimal_nrounds
  )
  
  # obtained fitted values
  data_do_x0 <- data_do_x1 <- data
  data_do_x0[[X]] <- 0
  data_do_x1[[X]] <- 1
  
  prob <- predict(model, dtrain)
  probx0 <- predict(model, xgb.DMatrix(data = as.matrix(data_do_x0[, c(X, Z, W)])))
  probx1 <- predict(model, xgb.DMatrix(data = as.matrix(data_do_x1[, c(X, Z, W)])))
  
  # get the propensity scores - regress X on Z, Z+W
  mod_xz <- glm(paste(X, "~", paste(Z, collapse = "+")), family = "binomial",
                data = data)
  px_z <- mod_xz$fitted.values
  mod_xzw <- glm(paste(X, "~", paste(c(Z, W), collapse = "+")), 
                 family = "binomial", data = data)
  px_zw <- mod_xzw$fitted.values
  px <- mean(data[[X]])
  
  # get f_{x_1, W_{x_0}}
  wgh0 <- (1 - px) / (1 - px_z)
  fx1_wx0 <- wgh_sum(probx1[data[[X]] == 0], wgh0[data[[X]] == 0])
  rx1_wx0 <- wgh_sum(thresh(probx1[data[[X]] == 0], thr), wgh0[data[[X]] == 0])
  
  # get f_{x_0, W_{x_0}}
  fx0_wx0 <- wgh_sum(probx0[data[[X]] == 0], wgh0[data[[X]] == 0])
  rx0_wx0 <- wgh_sum(thresh(probx0[data[[X]] == 0], thr), wgh0[data[[X]] == 0])
  
  # get f_{x_1, W_{x_1}}
  wgh1 <- px / px_z
  fx1_wx1 <- wgh_sum(probx1[data[[X]] == 1], wgh1[data[[X]] == 1])
  rx1_wx1 <- wgh_sum(thresh(probx1[data[[X]] == 1], thr), wgh1[data[[X]] == 1])
  
  # get f | x0
  f_x0 <- mean(prob[data[[X]] == 0])
  r_x0 <- mean(thresh(prob[data[[X]] == 0], thr))
  
  # get f | x1
  f_x1 <- mean(prob[data[[X]] == 1])
  r_x1 <- mean(thresh(prob[data[[X]] == 1], thr))
  
  # contributions to different effects
  base_de = fx1_wx0 - fx0_wx0
  base_ie = fx1_wx0 - fx1_wx1
  base_se = f_x1 - fx1_wx1 - (f_x0 - fx0_wx0)
  list(
    base_de = base_de, mtg_de = rx1_wx0 - rx0_wx0 - base_de,
    base_ie = base_ie, mtg_ie = rx1_wx0 - rx1_wx1 - base_ie,
    base_se = base_se, mtg_se = r_x1 - rx1_wx1 - (r_x0 - rx0_wx0) - base_se,
    u_de_base = probx1[data[[X]] == 0] - probx0[data[[X]] == 0],
    u_de_tot = thresh(probx1[data[[X]] == 0], thr) - 
               thresh(probx0[data[[X]] == 0], thr),
    u_de_wgh = wgh0[data[[X]] == 0]
  )
}

sample_influence <- function(total, base, wgh, effect = "DE") {
  
  data <- data.frame(total = total * wgh, base = base * wgh, 
                     mtg = (total - base) * wgh, 
                     sample = seq_along(total))
  data <- data[order(data$total, data$base, decreasing = TRUE),]
  data$total <- NULL
  data$order <- seq_len(nrow(data))
  ggplot(reshape2::melt(data, id.vars = c("sample", "order")), 
         aes(x = order, y = value, fill = variable)) +
    theme_bw() +
    geom_col(alpha = 0.5) +
    scale_fill_manual(name = "Component", labels = c("Base", "MTG"), 
                      values = c("gray", "red")) +
    xlab("Sample Ranking") + ylab("Sample Influence") +
    theme(legend.position = "bottom")
}

vis_mtg <- function(mtg) {
  
  data <- data.frame(
    c(mtg$base_de, mtg$mtg_de, mtg$base_ie, mtg$mtg_ie, mtg$base_se, mtg$mtg_se),
    rep(c("Base", "MTG"), 3),
    rep(c("Direct", "Indirect", "Spurious"), each = 2)
  )
  
  names(data) <- c("value", "variable", "effect")
  p_eff <- ggplot(data, aes(x = effect, y = value, fill = variable)) +
    theme_bw() +
    geom_col(alpha = 0.5, color = "black") +
    scale_fill_manual(name = "Component", labels = c("Base", "MTG"), 
                      values = c("gray", "red")) +
    xlab("Causal Effect") + ylab("Effect Size") +
    theme(legend.position = "bottom")
  
  p_samps <- sample_influence(mtg$u_de_tot, mtg$u_de_base, mtg$u_de_wgh)
  
  cowplot::plot_grid(p_eff, p_samps, ncol = 2L)
}

mtg_wrap <- function(dataset) {
  
  if (dataset == "compas") {
    
    # COMPAS example
    sfm <- SFM_proj("compas")
    
    # pre-process COMPAS data
    data <- compas
    data$race <- as.integer(data$race == "White")
    data$sex <- as.integer(data$sex == "Male")
    data$c_charge_degree <- as.integer(data$c_charge_degree == "F")
  } else if (dataset == "census") {
    
    c(data, sfm) %<-% preproc_census(gov_census, SFM_proj("census"))
  } else if (dataset == "mimic") {
    
    c(data, sfm) %<-% preproc_miiv()
  }
  
  mtg <- mind_the_gap(X = sfm$X, Y = sfm$Y, W = sfm$W, Z = sfm$Z, data = data, 
                      x0 = 0, x1 = 1, thr = 0.5)
  
  mtg
}
