# install_mylibs.R: R Library Installation Configuration
#
#  Description:
#  This file lists the required R packages for installation.
#  It is intended to be sourced by an installation script.
#
#  Usage:
#  To install these packages, use an R installation script that sources this file.
#
#  Example installation script:
#      source("$SCRIPTS/etc/install_mylibs.R")
#      install.packages(required_packages, dependencies=TRUE)

# Set CRAN repository
options(repos = "https://cloud.r-project.org/")

# List of required R packages
required_packages <- c(
    # Machine Learning & Statistics
    "randomForest", "forecast", "party", "gbm", "e1071", "lme4", "qcc", "DMwR2",

    # Data Manipulation & Visualization
    "plyr", "reshape2", "lubridate", "sqldf", "ggplot2", "stringr",

    # Time Series Analysis
    "TTR", "tseries", "xts", "zoo",

    # Geospatial & Mapping
    "XML", "httr", "curl", "ggmap", "classInt", "RColorBrewer",
    "rgeos", "maptools", "spsurvey", "mapproj", "maps",

    # General Utilities
    "readxl", "caTools", "aod", "car", "faraway", "scatterplot3d",
    "pracma", "ROI", "vegan", "xtable",

    # Additional Packages
    "lavaan", "igraph", "kernlab", "mvtnorm", "plotrix", "pequod"
)
