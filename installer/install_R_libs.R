########################################################################
# install_R_libs.R: R Library Installation Worker
#
#  Description:
#  Validate the selected R library and install configured R packages.
#  This file is invoked by install_R_libs.sh, not executed directly.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#    R --no-save --no-restore \
#      --file="$SCRIPTS/installer/install_R_libs.R" \
#      --args "$SCRIPTS/installer/install_mylibs.R"
#
#  Requirements:
#  - An R interpreter and one trusted, sourceable configuration file.
#  - An existing, writable first R library path for the running R process.
#
#  Exit Status:
#  0. R processing completed successfully.
#  1. Invalid argument, invalid library target, or R execution failure.
#
#  Notes:
#  - The shell wrapper selects R through sudo, using sudo's command path.
#  - The configuration is read from exactly one argument after --args.
#  - Both R files are interpreted in the caller's R process. When invoked
#    through the wrapper, they are evaluated with elevated privileges.
#  - Use trusted, access-controlled repository files and parent directories.
#
#  Version History:
#  v1.0 2026-10-20
#       Initial release.
#
########################################################################

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || is.na(args[[1L]]) || !nzchar(args[[1L]])) {
    stop("Exactly one R package configuration path is required.")
}
config_file <- args[[1L]]

cat("[INFO] R installation:", R.home(), "\n")

lib <- .libPaths()[1L]
if (length(lib) != 1L ||
    is.na(lib) ||
    !nzchar(lib) ||
    substr(lib, 1L, 1L) != "/" ||
    !dir.exists(lib) ||
    file.access(lib, 2L) != 0L) {
    stop("R library installation target is not writable")
}

cat("[INFO] Target library:", lib, "\n")
source(config_file)
install.packages(required_packages, dependencies = TRUE, lib = lib)
