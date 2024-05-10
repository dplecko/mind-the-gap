
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

plts <- list(
  vis_mtg(mtg_wrap("mimic"), "counterfactual", "mimic", "list"),
  vis_mtg(mtg_wrap("census"), "counterfactual", "census", "list")
)

# create a single figure with 4 plots
cowplot::plot_grid(
  plts[[1]][[1]], plts[[1]][[2]], plts[[2]][[1]], plts[[2]][[2]],
  ncol = 2L, labels = c("(a)", "(b)", "(c)", "(d)")
)

ggsave(file.path(root, "data", "census-mimic-mtgs.png"),
       width = 10, height = 7)
