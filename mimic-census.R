
root <- rprojroot::find_root(rprojroot::is_git_root)
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

set.seed(2024)
for (dataset in c("mimic", "census")) {
  
  plts <- vis_mtg(mtg_wrap("mimic"), "counterfactual", dataset, "list")
  mtg_plt <- cowplot::plot_grid(
    plts[[1]], plts[[2]], ncol = 2L, labels = c("(a)", "(b)")
  )
  ggsave(file.path(root, "data", paste0(dataset, "-mtg.png")),
         width = 10, height = 3.5)
}
