

root <- "~/fairness/mind-the-gap"
r_dir <- file.path(root, "R")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))


for (dataset in c("compas", "census", "mimic")) {
  
  mtg <- mtg_wrap(dataset)
  vis_mtg(mtg)
}