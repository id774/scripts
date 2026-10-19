#!/bin/sh

########################################################################
# install_npm.sh: Bulk npm Package Install Script
#
#  Description:
#  This script automates the installation of a compact set of globally
#  useful Node.js and TypeScript command-line tools. It updates npm itself
#  to the latest version and installs general development tools, formatters,
#  linters, framework and database CLIs, and language servers globally.
#  All npm installations are executed through Aikido Safe Chain.
#  This is a batch installer, so a failure of one package does not stop the
#  remaining packages.
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
#  Specify a Safe Chain executable outside the default location:
#      ./install_npm.sh /opt/node/24 --safe-chain /path/to/safe-chain
#
#  Use the Node.js and npm in PATH with a custom Safe Chain executable:
#      ./install_npm.sh --safe-chain /path/to/safe-chain
#
#  Options:
#  -h, --help        Display this help message and exit.
#  -v, --version     Display this help message and exit.
#  --safe-chain PATH Specify the Safe Chain executable.
#
#  Requirements:
#  - Node.js and npm must already be installed.
#  - Aikido Safe Chain must be installed and executable.
#  - By default, Safe Chain is discovered under
#    /opt/safe-chain/<VERSION>/bin/safe-chain.
#  - Network connectivity is required for npm registry access.
#  - The caller must have permission to write to the selected npm global prefix.
#  - The Safe Chain runtime prefix must be writable by the caller.
#
#  Exit Status:
#  0   Batch processing completed, including runs with individual package warnings,
#      or usage/help/version was displayed.
#  1   Safe Chain discovery was ambiguous or initialization failed.
#  126 A required command exists but is not executable.
#  127 A required command was not found.
#
#  Notes:
#  - If no prefix is provided, the Node.js and npm in PATH and the normal
#    npm global prefix are used.
#  - If a prefix is provided, the Node.js and npm under that prefix are used,
#    and global packages are installed into the same prefix.
#  - Safe Chain is required and is never bypassed or replaced with direct npm.
#  - If multiple Safe Chain installations are found under /opt/safe-chain,
#    the caller must explicitly select one using --safe-chain.
#  - Shell aliases and Safe Chain shell integration are not required.
#  - This script does not run sudo.
#  - A failure to update npm or install one package is reported as a warning,
#    and the remaining packages are still attempted.
#  - Proxy configuration is handled by Safe Chain and npm.
#
#  Version History:
#  v1.2 2026-10-19
#       Require Safe Chain for all npm installs (incompatible change),
#       with default /opt discovery and an explicit path override.
#  v1.1 2026-09-27
#       Add global formatting, linting, framework, Prisma, and language-server
#       tools and document package-order guidance.
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

# Resolve the Safe Chain executable
setup_safe_chain() {
    SAFE_CHAIN="$1"

    if [ -z "$SAFE_CHAIN" ]; then
        # Discover a versioned Safe Chain installation under /opt.
        for candidate in /opt/safe-chain/*/bin/safe-chain; do
            [ -f "$candidate" ] || continue

            if [ -n "$SAFE_CHAIN" ]; then
                echo "[ERROR] Multiple Safe Chain installations found under /opt/safe-chain." >&2
                echo "[ERROR] Specify the executable using --safe-chain PATH." >&2
                exit 1
            fi

            SAFE_CHAIN="$candidate"
        done
    fi

    if [ -z "$SAFE_CHAIN" ] || [ ! -f "$SAFE_CHAIN" ]; then
        echo "[ERROR] Safe Chain was not found." >&2
        echo "[ERROR] Install it under /opt/safe-chain/<VERSION>/bin/safe-chain or specify --safe-chain PATH." >&2
        exit 127
    fi

    if [ ! -x "$SAFE_CHAIN" ]; then
        echo "[ERROR] Safe Chain '$SAFE_CHAIN' is not executable." >&2
        exit 126
    fi

    # Verify that Safe Chain can start before attempting npm installations.
    if ! "$SAFE_CHAIN" --version > /dev/null; then
        echo "[ERROR] Failed to initialize Safe Chain '$SAFE_CHAIN'." >&2
        exit 1
    fi
}

# Run a global npm installation through Safe Chain
run_global_install() {
    if [ -n "$PREFIX" ]; then
        "$SAFE_CHAIN" npm install -g --prefix "$PREFIX" "$1"
    else
        "$SAFE_CHAIN" npm install -g "$1"
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
    # Roughly order general-purpose tools before larger frameworks and server tools.
    # Put external-service clients last when practical; the order is only a guide.
    # Define the list of packages as a multi-line string
    packages="
typescript
tsx
npm-check-updates
prettier
eslint
cspell
sql-formatter
prisma
@nestjs/cli
typescript-language-server
@prisma/language-server
@tailwindcss/language-server
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

    NODE_PREFIX=""
    SAFE_CHAIN_PATH=""

    # Preserve the existing optional Node.js prefix argument.
    if [ $# -gt 0 ]; then
        case "$1" in
            --safe-chain) ;;
            -*) usage ;;
            *)
                NODE_PREFIX="$1"
                shift
                ;;
        esac
    fi

    # Accept an optional explicit Safe Chain executable path.
    if [ $# -gt 0 ]; then
        [ "$1" = "--safe-chain" ] || usage
        shift

        [ $# -eq 1 ] && [ -n "$1" ] || usage
        SAFE_CHAIN_PATH="$1"
        shift
    fi

    [ $# -eq 0 ] || usage

    echo "[INFO] Starting npm package installation..."
    setup_environment "$NODE_PREFIX"
    setup_safe_chain "$SAFE_CHAIN_PATH"
    install_packages

    echo "[INFO] npm package installation processing completed."
    return 0
}

# Execute main function
main "$@"
