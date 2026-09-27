#!/bin/sh

########################################################################
# install_miniforge.sh: Installer for Miniforge
#
#  Description:
#  This script provides a Conda and Mamba runtime by installing Miniforge:
#  - Downloading the selected Miniforge release installer from the official
#    GitHub Release.
#  - Running that installer in batch mode to install the specified or
#    default version under a versioned prefix.
#  It targets Linux on x86_64 and aarch64. It is not a Python library bulk
#  installer; use install_conda.sh to add libraries to a Conda environment.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default Miniforge version:
#      ./install_miniforge.sh
#
#  Specify a different Miniforge version:
#      ./install_miniforge.sh VERSION
#
#  Specify an installation prefix:
#      ./install_miniforge.sh VERSION PREFIX
#
#  Install without sudo (for local user installation):
#      ./install_miniforge.sh VERSION PREFIX --no-sudo
#
#  Options:
#  -h, --help     Display this help message and exit.
#  -v, --version  Display this help message and exit.
#  --no-sudo      Install without sudo. Use it as the third argument,
#                 following the version and the installation prefix.
#
#  Notes:
#  - The default version is a fixed value selected when this installer was
#    last maintained. It is not automatically kept in sync with the latest
#    upstream release.
#  - An explicit VERSION is downloaded from its version-specific GitHub
#    Release. The corresponding release and Linux installer asset must exist.
#  - By default, if no installation path is provided, Miniforge will be
#    installed under /opt/conda/<major>.<minor>, taken from VERSION.
#  - An existing prefix is not updated or overwritten. If the official
#    installer refuses the prefix, the installation fails.
#  - PATH, shell configuration, and global symlinks are not changed, and
#    conda init is not run.
#
#  Requirements:
#  - Linux on x86_64 or aarch64, with network access to GitHub Releases.
#  - The commands curl, awk, uname, mkdir, rm, and bash. bash runs the
#    official Miniforge installer.
#  - Sudo privileges are required for the normal installation with sudo.
#
#  Exit Status:
#  0   Installation succeeded, or usage/help/version was displayed.
#  1   Unsupported platform/architecture, sudo privilege failure,
#      download failure, installation failure, verification failure,
#      staging/cleanup failure, or other general installation failure.
#  126 A required command exists but is not executable.
#  127 A required command was not found.
#
#  Version History:
#  v1.0 2026-09-27
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version, prefix, sudo mode, and Miniforge installer asset
setup_environment() {
    VERSION="${1:-26.7.2-0}"
    SERIES="$(echo "$VERSION" | awk -F. '{print $1 "." $2}')"
    PREFIX="${2:-/opt/conda/$SERIES}"
    if [ "$3" = "--no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi

    SYSTEM="$(uname -s)"
    if [ "$SYSTEM" != "Linux" ]; then
        echo "[ERROR] Unsupported operating system: $SYSTEM" >&2
        exit 1
    fi

    MACHINE="$(uname -m)"
    case "$MACHINE" in
        x86_64|amd64)
            ARCH="x86_64"
            ;;
        aarch64|arm64)
            ARCH="aarch64"
            ;;
        *)
            echo "[ERROR] Unsupported architecture: $MACHINE" >&2
            exit 1
            ;;
    esac

    ASSET="Miniforge3-$VERSION-Linux-$ARCH.sh"
}

# Download and run the official Miniforge installer in batch mode
install_miniforge() {
    echo "[INFO] Creating temporary build directory."
    if ! mkdir install_miniforge; then
        echo "[ERROR] Failed to create install_miniforge directory." >&2
        exit 1
    fi

    echo "[INFO] Downloading Miniforge $VERSION ($ASSET)."
    if ! curl -fL "https://github.com/conda-forge/miniforge/releases/download/$VERSION/$ASSET" -o "install_miniforge/$ASSET"; then
        echo "[ERROR] Failed to download Miniforge $VERSION ($ASSET)." >&2
        exit 1
    fi

    # Batch mode installs without prompts and without shell initialization;
    # -u is not given, so the installer refuses an existing prefix.
    echo "[INFO] Installing Miniforge to $PREFIX."
    if ! $SUDO bash "install_miniforge/$ASSET" -b -p "$PREFIX"; then
        echo "[ERROR] Failed to install Miniforge to $PREFIX." >&2
        exit 1
    fi
}

# Verify that the installed Conda and Mamba run and Conda reports the expected version
verify_installation() {
    echo "[INFO] Verifying installed Conda and Mamba."
    # Miniforge release versions carry a build suffix, e.g. 26.7.2-0 ships conda 26.7.2.
    EXPECTED_CONDA="conda ${VERSION%-*}"
    if ! INSTALLED_CONDA="$("$PREFIX/bin/conda" --version)" || [ "$INSTALLED_CONDA" != "$EXPECTED_CONDA" ]; then
        echo "[ERROR] Conda verification failed: expected '$EXPECTED_CONDA', got '$INSTALLED_CONDA'." >&2
        exit 1
    fi

    # The Mamba version is not derived from the Miniforge release, so only require it to run.
    if ! INSTALLED_MAMBA="$("$PREFIX/bin/mamba" --version)"; then
        echo "[ERROR] Mamba verification failed: $PREFIX/bin/mamba --version did not succeed." >&2
        exit 1
    fi
    echo "[INFO] Installed $INSTALLED_CONDA and mamba $INSTALLED_MAMBA."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    if [ $# -gt 3 ] || { [ $# -eq 3 ] && [ "$3" != "--no-sudo" ]; }; then
        usage
    fi
    case "$1" in
        -*) usage ;;
    esac
    case "$2" in
        -*) usage ;;
    esac

    # Perform initial checks
    check_commands curl awk uname mkdir rm bash
    setup_environment "$@"
    # No-sudo mode must not depend on sudo; validate sudo only in sudo mode.
    if [ "$SUDO" = "sudo" ]; then
        check_sudo
    fi

    # Run the installation process
    install_miniforge
    verify_installation

    echo "[INFO] Cleaning up temporary files."
    if ! rm -rf install_miniforge; then
        echo "[ERROR] Failed to remove temporary build directory." >&2
        exit 1
    fi

    echo "[INFO] Miniforge $VERSION installed successfully in $PREFIX."
    echo "[INFO] Conda command: $PREFIX/bin/conda"
    echo "[INFO] Mamba command: $PREFIX/bin/mamba"
    echo "[INFO] PATH and shell configuration were not changed, and conda init was not run."
    return 0
}

# Execute main function
main "$@"
