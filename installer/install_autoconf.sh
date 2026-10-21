#!/bin/sh

########################################################################
# install_autoconf.sh: Installer for Autoconf
#
#  Description:
#  This script automates the installation of Autoconf by:
#  - Downloading the specified or default version from the GNU FTP server.
#  - Compiling and installing it to the system.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default version:
#      ./install_autoconf.sh
#
#  Specify a version to install a different release:
#      ./install_autoconf.sh VERSION
#
#  Skip saving sources by specifying any second argument:
#      ./install_autoconf.sh VERSION skip
#
#  Skip the confirmation prompt by specifying --yes as the last argument:
#      ./install_autoconf.sh VERSION skip --yes
#
#  Notes:
#  - The default version is a provisional convenience value used when no
#    version is specified. It normally reflects the current release selected
#    when this installer was last maintained and is not automatically kept in
#    sync with upstream releases.
#  - An explicit version is resolved through this installer's configured
#    download source and path rules. It can be installed only when the
#    corresponding artifact is available from that source.
#  - After downloading, the SHA-256 of the file is displayed and y/yes
#    approval is required before it is processed. Any other response, or an
#    unavailable terminal, aborts the installation.
#  - The optional --yes argument skips only the confirmation prompt; the
#    SHA-256 is still displayed. It is accepted only as the last argument.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, and `sudo` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
#  - The user must have `sha256sum` or `shasum` installed.
#
#  Version History:
#  v1.1 2026-10-21
#       Show downloaded file SHA-256 and confirm before install, with --yes bypass.
#  v1.0 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v0.9 2026-02-05
#       Remove preflight network connectivity check.
#  v0.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v0.7 2025-04-27
#       Add error handling for critical installation steps to ensure robust build and installation.
#  v0.6 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v0.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-20
#       Update default Autoconf version to 2.72 and adjust installation prefix.
#  v0.2 2025-03-14
#       Added network connection check and improved argument handling.
#  v0.1 2011-04-26
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

# Check if the system is Linux
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
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

# Setup version environment
setup_environment() {
    VERSION="${1:-2.72}"
}

# Save sources if requested
save_sources() {
    echo "[INFO] Saving sources to /usr/local/src/autoconf."
    if ! sudo mkdir -p /usr/local/src/autoconf; then
        echo "[ERROR] Failed to create /usr/local/src/autoconf." >&2
        exit 1
    fi
    if ! sudo cp -av "autoconf-$VERSION" /usr/local/src/autoconf/; then
        echo "[ERROR] Failed to copy autoconf-$VERSION to /usr/local/src/autoconf." >&2
        exit 1
    fi
    sudo chown -R root:root /usr/local/src/autoconf
}

# Install Autoconf
install_autoconf() {
    setup_environment "$1"

    echo "[INFO] Creating temporary build directory."
    mkdir install_autoconf
    cd install_autoconf || exit 1

    echo "[INFO] Downloading Autoconf $VERSION."
    if ! wget "ftp://ftp.gnu.org/gnu/autoconf/autoconf-$VERSION.tar.gz"; then
        echo "[ERROR] Failed to download autoconf-$VERSION.tar.gz." >&2
        exit 1
    fi

    show_sha256 "autoconf-$VERSION.tar.gz"
    [ "$ASSUME_YES" = "true" ] || confirm_installation

    echo "[INFO] Extracting archive."
    if ! tar xzvf "autoconf-$VERSION.tar.gz"; then
        echo "[ERROR] Failed to extract autoconf-$VERSION.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Configuring build."
    cd "autoconf-$VERSION" || exit 1
    if ! ./configure --prefix=/usr/local; then
        echo "[ERROR] Failed to configure autoconf-$VERSION." >&2
        exit 1
    fi

    echo "[INFO] Building Autoconf."
    if ! make; then
        echo "[ERROR] Failed to build autoconf-$VERSION." >&2
        exit 1
    fi

    echo "[INFO] Installing Autoconf."
    if ! sudo make install; then
        echo "[ERROR] Failed to install autoconf-$VERSION." >&2
        exit 1
    fi

    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1

    echo "[INFO] Cleaning up temporary files."
    rm -rf install_autoconf
}

# Display the SHA-256 of a downloaded file
show_sha256() {
    echo "[INFO] Downloaded file: ${1##*/}"
    if command -v sha256sum >/dev/null 2>&1; then
        sha256_line=$(sha256sum "$1")
    elif command -v shasum >/dev/null 2>&1; then
        sha256_line=$(shasum -a 256 "$1")
    else
        echo "[ERROR] Command 'sha256sum' or 'shasum' is not installed. Please install one of them and try again." >&2
        exit 127
    fi
    sha256_status=$?
    sha256_value=${sha256_line%% *}
    case "$sha256_value" in
        *[!0-9a-f]*) sha256_status=1 ;;
    esac
    if [ "$sha256_status" -ne 0 ] || [ "${#sha256_value}" -ne 64 ]; then
        echo "[ERROR] Failed to calculate SHA-256 of $1." >&2
        exit 1
    fi
    echo "[INFO] SHA-256: $sha256_value"
}

# Ask the user to approve processing of the downloaded file
confirm_installation() {
    printf '[INFO] Proceed with installation? [y/N]: '
    if ! read -r response < /dev/tty; then
        echo >&2
        echo "[ERROR] Failed to read the confirmation from the terminal." >&2
        exit 1
    fi

    case "$response" in
        y|Y|yes|YES) ;;
        *)
            echo "[ERROR] Installation aborted by user." >&2
            exit 1
            ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Accept --yes only as the last argument and remove it from the arguments
    ASSUME_YES=false
    last_arg=
    for arg in "$@"; do
        last_arg="$arg"
    done
    if [ "$last_arg" = "--yes" ]; then
        ASSUME_YES=true
        i=1
        while [ "$i" -lt "$#" ]; do
            set -- "$@" "$1"
            shift
            i=$((i + 1))
        done
        shift
    fi
    for arg in "$@"; do
        if [ "$arg" = "--yes" ]; then
            usage
        fi
    done

    # Perform initial checks
    check_system
    check_commands wget make tar mkdir cp rm chown
    check_sudo

    # Run the installation process
    install_autoconf "$1" "$2"

    echo "[INFO] Autoconf $VERSION installed successfully."
    return 0
}

# Execute main function
main "$@"
