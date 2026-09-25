#!/bin/sh

########################################################################
# install_npm.sh: Bulk npm Package Install Script
#
#  Description:
#  This script automates the installation of a compact set of globally
#  useful Node.js and TypeScript command-line tools. It updates npm itself
#  to the latest version and globally installs TypeScript, tsx, and
#  npm-check-updates. This is a batch installer, so a failure of one
#  package does not stop the remaining packages.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Use the Node.js and npm found in PATH:
#      ./install_npm.sh
#
#  Use the Node.js and npm installed under a Node.js installation prefix:
#      ./install_npm.sh /opt/node/24
#
#  Options:
#  -h, --help     Display this help message and exit.
#  -v, --version  Display this help message and exit.
#
#  Requirements:
#  - Node.js and npm must already be installed.
#  - Network connectivity is required for npm registry access.
#  - The caller must have permission to write to the selected npm global prefix.
#
#  Exit Status:
#  0   Batch processing completed, including runs with individual package warnings,
#      or usage/help/version was displayed.
#  126 A required command exists but is not executable.
#  127 A required command was not found.
#
#  Notes:
#  - If no prefix is provided, the Node.js and npm in PATH and the normal
#    npm global prefix are used.
#  - If a prefix is provided, the Node.js and npm under that prefix are used,
#    and global packages are installed into the same prefix.
#  - This script does not run sudo.
#  - A failure to update npm or install one package is reported as a warning,
#    and the remaining packages are still attempted.
#  - Proxy configuration is handled by npm from the inherited environment.
#
#  Version History:
#  v1.0 2026-09-25
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Set up the Node.js and npm commands and the optional installation prefix
setup_environment() {
    PREFIX="$1"
    if [ -n "$PREFIX" ]; then
        # npm resolves node through '#!/usr/bin/env node', so put the selected
        # installation first in PATH to run npm with the same node.
        PATH="$PREFIX/bin:$PATH"
        export PATH
        NODE="$PREFIX/bin/node"
        NPM="$PREFIX/bin/npm"
    else
        NODE=node
        NPM=npm
    fi

    # Verify that node and npm are available
    check_commands "$NODE" "$NPM"
}

# Run a global npm installation, keeping it in the selected prefix if any
run_global_install() {
    if [ -n "$PREFIX" ]; then
        "$NPM" install -g --prefix "$PREFIX" "$1"
    else
        "$NPM" install -g "$1"
    fi
}

# Install a single npm package
install_package() {
    echo "[INFO] Installing $1..."
    if ! run_global_install "$1"; then
        echo "[WARN] Failed to install $1; continuing." >&2
    fi
    return 0
}

# Update npm and install the selected npm packages
install_packages() {
    echo "[INFO] Updating npm to the latest version..."
    if ! run_global_install npm@latest; then
        echo "[WARN] Failed to update npm; continuing." >&2
    fi

    echo "[INFO] Installing selected npm development tools..."
    # Define the list of packages as a multi-line string
    packages="
typescript
tsx
npm-check-updates
"

    # Loop through each package and install it
    for package in $packages; do
        install_package "$package"
    done
    return 0
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ $# -gt 1 ]; then
        usage
    fi
    case "$1" in
        -*) usage ;;
    esac

    echo "[INFO] Starting npm package installation..."
    setup_environment "$1"
    install_packages

    echo "[INFO] npm package installation processing completed."
    return 0
}

# Execute main function
main "$@"
