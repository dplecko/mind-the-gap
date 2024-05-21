
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

set.seed(2024)

# load COMPAS
c(data, sfm) %<-% preproc_compas()

# run the algorithm on the optimal predictor
mtg_opt <- mind_the_gap(data, X = sfm$X, Z = sfm$Z, W = sfm$W, Y = sfm$Y,
                        nboot = 50)

# run the algorithm on the Northpointe predictor
mtg_np <- mind_the_gap(data, X = sfm$X, Z = sfm$Z, W = sfm$W, Y = sfm$Y, 
                       Shat = "np_shat", nboot = 50)

# Part A: Y vs Shat^NP decomposition
ctf_meas <- function(x, out) {
  
  data <- data.frame(
    rbind(x$ctfde_base, x$ctfie_base, x$ctfse_base),
    c("Ctf-DE", "Ctf-IE", "Ctf-SE")
  )
  names(data) <- c("value", "sd", "effect")
  setDT(data)
  data <- data[order(effect)]
  data[, `:=` (csum = cumsum(value)), by = effect]
  data[, `:=` (ymin = csum - sd, ymax = csum + sd)]
  data$Outcome <- out
  data
}

adat <- rbind(ctf_meas(mtg_opt, "Y"), ctf_meas(mtg_np, "Shat"))

aplot <- ggplot(adat, aes(x = effect, y = value, fill = Outcome)) +
  geom_col(alpha = 0.5, position = "dodge", color = "black") + theme_bw() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.4,
                color = "darkgrey", position = position_dodge(0.9)) +
  theme(legend.position = "bottom") +
  xlab("Causal Effect") + ylab("Effect Size") +
  scale_fill_manual(labels = c(TeX("$\\hat{S}^{NP}$"), TeX("$Y$")),
                    values = c("#00BFC4", "#7CAE00")) +
  ggtitle(TeX("COMPAS $\\hat{S}^{NP}$ and $Y$ Decompositions"))
  
# Part B: Margin Complement M contributions
mtg_vis <- vis_mtg(mtg_np, "counterfactual", "compas", "list")
cowplot::plot_grid(
  aplot, mtg_vis[[1]], mtg_vis[[2]], labels = c("(a)", "(b)", "(c)"), ncol = 3
)

ggsave(file.path(root, "data", "compas-auditing.png"), width = 12, height = 4)
