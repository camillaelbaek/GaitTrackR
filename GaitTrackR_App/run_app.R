# run_app.R — one-click launcher backend
# Installs any missing packages, then launches the Scenith Shiny app.

pkgs <- c(
  "shiny", "dplyr", "tidyr", "ggplot2", "DT", "stringr", "purrr", "scales",
  "readr", "readxl",                          # metadata CSV / XLSX support
  "ggridges", "ggpubr", "viridis", "ggbeeswarm", "sp","ggprism"
)

# CRAN packages
cran_pkgs <- c("shiny","dplyr","tidyr","ggplot2","DT","stringr","purrr",
               "scales","readr","readxl","ggridges","ggpubr","viridis",
               "ggbeeswarm","sp")
bioc_pkgs <- c("ggprism")

need_cran <- cran_pkgs[!cran_pkgs %in% rownames(installed.packages())]
if (length(need_cran)) {
  message("Installing missing CRAN packages: ", paste(need_cran, collapse = ", "))
  install.packages(need_cran, repos = "https://cloud.r-project.org")
}

need_bioc <- bioc_pkgs[!bioc_pkgs %in% rownames(installed.packages())]
if (length(need_bioc)) {
  message("Installing missing Bioconductor packages: ", paste(need_bioc, collapse = ", "))
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  BiocManager::install(need_bioc, ask = FALSE)
}

# Run the app in this folder and open browser
shiny::runApp(".", launch.browser = TRUE)
