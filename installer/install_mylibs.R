########################################################################
# install_mylibs.R: R Package Installation Configuration
#
#  Description:
#  Define the CRAN repository and required R package names for the
#  installer/install_R_libs.R package installation worker.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#    Loaded by installer/install_R_libs.R through source(config_file).
#
#  Requirements:
#  - R is required. The file must remain valid R code that can be sourced.
#
#  Notes:
#  - Defines required_packages and the CRAN repository. It does not
#    perform package installation when sourced by itself.
#  - Loaded by R running under sudo during the normal installation flow.
#  - The file and its parent directories must be trusted and access controlled.
#  - This is a configuration source file, not a standalone executable.
#
#  Version History:
#  v1.0 2026-10-20
#       Initial release.
#
########################################################################

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
