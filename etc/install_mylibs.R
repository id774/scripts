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
options(repos="http://cran.ism.ac.jp")

# List of required R packages
required_packages <- c(
    # Machine Learning & Statistics
    "randomForest", "forecast", "party", "gbm", "e1071", "lme4", "qcc", "DMwR",

    # Data Manipulation & Visualization
    "plyr", "reshape2", "lubridate", "sqldf", "ggplot2", "stringr",

    # Time Series Analysis
    "TTR", "tseries", "xts", "zoo",

    # Geospatial & Mapping
    "XML", "RCurl", "RgoogleMaps", "classInt", "RColorBrewer",
    "gpclib", "maptools", "spsurvey", "mapproj", "maps",

    # General Utilities
    "XLConnect", "caTools", "aod", "car", "faraway", "scatterplot3d",
    "pracma", "quadprog", "vegan", "xtable",

    # Additional Packages
    "sem", "igraph", "kernlab", "mvtnorm", "plotrix", "pequod"
)
