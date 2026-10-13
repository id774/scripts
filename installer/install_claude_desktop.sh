#!/bin/sh

########################################################################
# install_claude_desktop.sh: Install Claude Desktop on Debian/Ubuntu
#
#  Description:
#  Install Claude Desktop from Anthropic's official APT repository.
#  Use APT to identify existing repositories instead of parsing APT sources.
#  Install the verified signing key and tracked source only when absent.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_claude_desktop.sh
#
#  Requirements:
#  - Debian 12+ or Ubuntu 22.04+ (amd64 or arm64).
#  - The invoking user must have sudo privileges.
#  - Repository template: ../etc/apt.conf.d/sources-claude-desktop.list.
#  - Internet connectivity for the initial key and package installation.
#
#  Exit Status:
#  0: Success or usage/help/version display.
#  1: Unsupported environment, configuration conflict, or installation failure.
#  126: Required command exists but is not executable.
#  127: Required command is not found.
#
#  Notes:
#  - Existing APT sources and signing keys are preserved, not overwritten.
#  - Signing key files must contain only the expected Anthropic primary key.
#  - The package may manage claude-desktop.list during installation or upgrades.
#  - Existing active APT sources are identified by APT itself.
#  - Installed packages are left to regular APT upgrades.
#
#  Version History:
#  v1.0 2026-10-13
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

# Check if the system is a supported Debian or Ubuntu release and architecture
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ] || [ ! -r /etc/os-release ]; then
        echo "[ERROR] Debian 12+ or Ubuntu 22.04+ is required." >&2
        return 1
    fi

    . /etc/os-release
    case "$ID" in
        debian) minimum_version=12 ;;
        ubuntu) minimum_version=22.04 ;;
        *)
            echo "[ERROR] Debian 12+ or Ubuntu 22.04+ is required." >&2
            return 1
            ;;
    esac

    if [ -z "${VERSION_ID:-}" ] ||
       ! dpkg --compare-versions "$VERSION_ID" ge "$minimum_version"; then
        echo "[ERROR] Unsupported Debian or Ubuntu release." >&2
        return 1
    fi

    architecture=$(dpkg --print-architecture) || return 1
    case "$architecture" in
        amd64|arm64) ;;
        *)
            echo "[ERROR] Unsupported Debian package architecture." >&2
            return 1
            ;;
    esac
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        return 1
    fi
}

# Resolve the tracked APT source template and system installation paths
setup_paths() {
    script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd) || return 1
    source_template="$script_dir/../etc/apt.conf.d/sources-claude-desktop.list"
    source_target=/etc/apt/sources.list.d/claude-desktop.list
    keyring=/usr/share/keyrings/claude-desktop-archive-keyring.asc
    repository_base=https://downloads.claude.ai/claude-desktop/apt/stable/dists/stable
    expected_fingerprint=31DDDE24DDFAB679F42D7BD2BAA929FF1A7ECACE

    if [ ! -r "$source_template" ]; then
        echo "[ERROR] Missing repository template: $source_template" >&2
        return 1
    fi
}

# Read the fingerprint of a GPG key file that holds exactly one primary key
key_fingerprint() {
    gpg --batch --show-keys --with-colons "$1" 2>/dev/null |
        awk -F: '
            $1 == "pub" || $1 == "sec" { count++; want = 1; next }
            $1 == "sub" || $1 == "ssb" { want = 0; next }
            $1 == "fpr" && want { fingerprint = $10; want = 0 }
            END { if (count == 1) print fingerprint }
        '
}

# Install Anthropic's signing key only after verifying its published fingerprint
install_signing_key() {
    if [ -e "$keyring" ] || [ -L "$keyring" ]; then
        if [ "$(key_fingerprint "$keyring")" != "$expected_fingerprint" ]; then
            echo "[ERROR] Existing Anthropic keyring differs: $keyring" >&2
            return 1
        fi
        echo "[INFO] Anthropic signing key is already installed."
        return 0
    fi

    check_commands curl
    temp_dir=$(mktemp -d) || return 1
    trap 'rm -rf "$temp_dir"' 0
    trap 'exit 1' 1 2 3 15

    echo "[INFO] Downloading Anthropic signing key..."
    if ! curl -fsSL https://downloads.claude.ai/claude-desktop/key.asc \
        -o "$temp_dir/claude-desktop-key.asc"; then
        echo "[ERROR] Failed to download Anthropic signing key." >&2
        return 1
    fi

    if [ "$(key_fingerprint "$temp_dir/claude-desktop-key.asc")" != "$expected_fingerprint" ]; then
        echo "[ERROR] Unexpected Anthropic signing key fingerprint." >&2
        return 1
    fi
    if ! sudo mkdir -p /usr/share/keyrings ||
       ! sudo install -o root -g root -m 0644 \
        "$temp_dir/claude-desktop-key.asc" "$keyring"; then
        echo "[ERROR] Failed to install Anthropic signing key." >&2
        return 1
    fi
    echo "[INFO] Installed Anthropic signing key."
}

# Use APT's active index targets instead of parsing source definitions
configure_repository() {
    if [ -L "$source_target" ]; then
        echo "[ERROR] Repository configuration is a symbolic link: $source_target" >&2
        return 1
    fi
    if [ -e "$source_target" ] &&
       { [ ! -f "$source_target" ] || [ ! -r "$source_target" ]; }; then
        echo "[ERROR] Invalid existing Claude Desktop APT source: $source_target" >&2
        return 1
    fi

    uris=$(apt-get --print-uris update) || {
        echo "[ERROR] Failed to inspect APT sources." >&2
        return 1
    }

    case "$uris" in
        *"'$repository_base/main/binary-$architecture/Packages"*)
            echo "[INFO] Claude Desktop APT repository is already configured."
            return 0
            ;;
        *"'$repository_base/InRelease'"*)
            echo "[ERROR] Existing Claude Desktop APT source does not provide $architecture packages." >&2
            return 1
            ;;
    esac

    if [ -e "$source_target" ]; then
        echo "[ERROR] Existing Claude Desktop APT source is not active: $source_target" >&2
        return 1
    fi

    if ! sudo mkdir -p /etc/apt/sources.list.d ||
       ! sudo install -o root -g root -m 0644 \
        "$source_template" "$source_target"; then
        echo "[ERROR] Failed to install Claude Desktop APT repository configuration." >&2
        return 1
    fi
    echo "[INFO] Configured Claude Desktop APT repository."
}

# Install Claude Desktop only when its Debian package is absent
install_claude_desktop() {
    echo "[INFO] Updating APT package metadata..."
    if ! sudo apt-get update; then
        echo "[ERROR] Failed to update APT package metadata." >&2
        return 1
    fi
    echo "[INFO] Installing Claude Desktop..."
    if ! sudo apt-get install -y claude-desktop; then
        echo "[ERROR] Failed to install Claude Desktop." >&2
        return 1
    fi
    echo "[INFO] Claude Desktop installation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
        "")
            [ "$#" -eq 0 ] || usage
            ;;
        *) usage ;;
    esac

    check_commands dpkg apt-get dpkg-query gpg awk grep \
                   install mkdir mktemp rm dirname
    check_system || return 1
    check_sudo || return 1
    setup_paths || return 1
    if dpkg-query -W -f='${Status}' claude-desktop 2>/dev/null |
       grep -q '^install ok installed$'; then
        echo "[INFO] Claude Desktop is already installed."
        return 0
    fi
    install_signing_key || return 1
    configure_repository || return 1
    install_claude_desktop || return 1
}

# Execute main function
main "$@"
