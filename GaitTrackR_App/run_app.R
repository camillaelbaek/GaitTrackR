# run_app.R — one-click launcher backend
pkgs <- c("shiny","dplyr","tidyr","ggplot2","readxl","writexl","DT","ggprism","ggpattern","ggrepel")

need <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(need)) {
  install.packages(need, repos = "https://cloud.r-project.org")
}

# Run the app in this folder and open browser
shiny::runApp(".", launch.browser = TRUE)

