# init.R
# This script will be run once when the Shiny app is deployed on ShinyApps.io
# It ensures all necessary packages are installed before the app starts

# List of required packages
my_packages <- c(
  "shiny", "tidyverse", "lubridate", "janitor", "sf", "glue", "scales",
  "ggplot2", "slider", "patchwork", "ggalt", "viridis", "kableExtra", "cowplot"
)

# Helper function to check and install packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Install all required packages
invisible(sapply(my_packages, install_if_missing))

# Verify installation
for (pkg in my_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Failed to install package:", pkg))
  }
}

# Let ShinyApps.io know we're done
cat("init.R executed successfully\n")
