# run_app.R — one-click launcher
# Installs any missing packages, then launches the GaitTrackR Shiny app.

cran_pkgs <- c(
  "shiny", "dplyr", "tidyr", "ggplot2", "DT", "stringr", "purrr",
  "scales", "readr", "readxl", "ggridges", "ggpubr", "viridis",
  "ggbeeswarm", "sp", "ggprism", "ggpattern", "writexl",
  "magick", "imager", "ggrepel"    # ← add ggrepel here
)

need_cran <- cran_pkgs[!cran_pkgs %in% rownames(installed.packages())]
if (length(need_cran)) {
  message("Installing missing packages: ", paste(need_cran, collapse = ", "))
  install.packages(need_cran, repos = "https://cloud.r-project.org")
}

shiny::runApp(".", launch.browser = TRUE)
