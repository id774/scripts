#!/bin/sh

########################################################################
# install_vscode.sh: Install Visual Studio Code on Debian/Ubuntu
#
#  Description:
#  Install Visual Studio Code Stable from Microsoft's official APT repository.
#  This script performs the following actions:
#    1) Validate the Debian-based system and sudo privileges.
#    2) Check for conflicting existing VS Code APT sources.
#    3) Install Microsoft's verified signing key when absent.
#    4) Deploy the repository configuration from the tracked template.
#    5) Refresh APT metadata and install the code package when needed.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_vscode.sh
#
#  Requirements:
#  - Debian or Ubuntu based Linux system (amd64, arm64, or armhf).
#  - The invoking user must have sudo privileges.
#  - Repository template: ../etc/apt.conf.d/sources-vscode.sources.
#  - Internet connectivity for the initial key and package installation.
#
#  Exit Status:
#  0: Success or usage/help/version display.
#  1: Unsupported environment, configuration conflict, or installation failure.
#  126: Required command exists but is not executable.
#  127: Required command is not found.
#
#  Notes:
#  - Existing APT repository entries and Microsoft signing keys are preserved.
#  - Reinstallation does not overwrite the repository configuration.
#  - Already installed packages are left to regular APT upgrades.
#
#  Version History:
#  v1.0 2026-10-12
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

# Check if the system is Debian or Ubuntu based Linux with a supported architecture
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ] || [ ! -r /etc/os-release ]; then
        echo "[ERROR] Debian or Ubuntu based Linux is required." >&2
        return 1
    fi

    . /etc/os-release
    case "$ID" in
        debian|ubuntu) ;;
        *)
            case " $ID_LIKE " in
                *" debian "*|*" ubuntu "*) ;;
                *)
                    echo "[ERROR] Debian or Ubuntu based Linux is required." >&2
                    return 1
                    ;;
            esac
            ;;
    esac

    case "$(dpkg --print-architecture)" in
        amd64|arm64|armhf) ;;
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
    source_template="$script_dir/../etc/apt.conf.d/sources-vscode.sources"
    source_target=/etc/apt/sources.list.d/vscode.sources
    keyring=/usr/share/keyrings/microsoft.gpg
    expected_fingerprint=BC528686B50D79E339D3721CEB3E94ADBE1229CF

    if [ ! -r "$source_template" ]; then
        echo "[ERROR] Missing repository template: $source_template" >&2
        return 1
    fi
}

# Refuse conflicting repository definitions without changing existing APT sources
check_repository_conflicts() {
    if [ -e "$source_target" ] || [ -L "$source_target" ]; then
        if ! cmp -s "$source_template" "$source_target"; then
            echo "[ERROR] Existing repository configuration differs: $source_target" >&2
            return 1
        fi
    fi

    # Do not add a second definition of the same Microsoft repository.
    for file in /etc/apt/sources.list /etc/apt/sources.list.d/*.list \
                /etc/apt/sources.list.d/*.sources; do
        [ -f "$file" ] || continue
        [ "$file" = "$source_target" ] && continue
        if [ ! -r "$file" ]; then
            echo "[ERROR] Cannot read APT repository file: $file" >&2
            return 1
        fi
        if awk '
            /^[[:space:]]*#/ { next }
            /^[[:space:]]*deb[[:space:]]/ && index($0, "packages.microsoft.com/repos/code") { found = 1 }
            /^[[:space:]]*URIs:[[:space:]]/ && index($0, "packages.microsoft.com/repos/code") { found = 1 }
            END { exit !found }
        ' "$file"; then
            echo "[ERROR] Another VS Code APT source exists: $file" >&2
            return 1
        fi
    done
}

# Read the first public key fingerprint from a GPG key file
key_fingerprint() {
    gpg --batch --show-keys --with-colons "$1" 2>/dev/null |
        awk -F: '$1 == "fpr" { print $10; exit }'
}

# Install Microsoft's signing key only after verifying its published fingerprint
install_signing_key() {
    if [ -e "$keyring" ] || [ -L "$keyring" ]; then
        if [ "$(key_fingerprint "$keyring")" != "$expected_fingerprint" ]; then
            echo "[ERROR] Existing Microsoft keyring differs: $keyring" >&2
            return 1
        fi
        echo "[INFO] Microsoft signing key is already installed."
        return 0
    fi

    check_commands curl
    temp_dir=$(mktemp -d) || return 1
    trap 'rm -rf "$temp_dir"' 0
    trap 'exit 1' 1 2 3 15

    echo "[INFO] Downloading Microsoft signing key..."
    if ! curl -fsSL https://packages.microsoft.com/keys/microsoft.asc \
        -o "$temp_dir/microsoft.asc"; then
        echo "[ERROR] Failed to download Microsoft signing key." >&2
        return 1
    fi
    if ! gpg --batch --dearmor < "$temp_dir/microsoft.asc" \
        > "$temp_dir/microsoft.gpg"; then
        echo "[ERROR] Failed to convert Microsoft signing key." >&2
        return 1
    fi

    # Reject an unexpected key instead of trusting a changed download.
    if [ "$(key_fingerprint "$temp_dir/microsoft.gpg")" != "$expected_fingerprint" ]; then
        echo "[ERROR] Unexpected Microsoft signing key fingerprint." >&2
        return 1
    fi
    if ! sudo mkdir -p /usr/share/keyrings ||
       ! sudo install -o root -g root -m 0644 "$temp_dir/microsoft.gpg" "$keyring"; then
        echo "[ERROR] Failed to install Microsoft signing key." >&2
        return 1
    fi
    echo "[INFO] Installed Microsoft signing key."
}

# Deploy the tracked APT source template without overwriting an existing file
configure_repository() {
    if [ -e "$source_target" ] || [ -L "$source_target" ]; then
        echo "[INFO] VS Code APT repository is already configured."
        return 0
    fi
    if ! sudo mkdir -p /etc/apt/sources.list.d ||
       ! sudo install -o root -g root -m 0644 \
        "$source_template" "$source_target"; then
        echo "[ERROR] Failed to install VS Code APT repository configuration." >&2
        return 1
    fi
    echo "[INFO] Configured VS Code APT repository."
}

# Install Visual Studio Code only when its Debian package is absent
install_vscode() {
    if dpkg-query -W -f='${Status}' code 2>/dev/null |
       grep -q '^install ok installed$'; then
        echo "[INFO] Visual Studio Code is already installed."
        return 0
    fi

    echo "[INFO] Updating APT package metadata..."
    if ! sudo apt-get update; then
        echo "[ERROR] Failed to update APT package metadata." >&2
        return 1
    fi
    echo "[INFO] Installing Visual Studio Code..."
    if ! sudo apt-get install -y code; then
        echo "[ERROR] Failed to install Visual Studio Code." >&2
        return 1
    fi
    echo "[INFO] Visual Studio Code installation completed."
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

    check_commands dpkg apt-get dpkg-query gpg awk cmp grep \
                   install mkdir mktemp rm dirname
    check_system || return 1
    check_sudo || return 1
    setup_paths || return 1
    check_repository_conflicts || return 1
    install_signing_key || return 1
    configure_repository || return 1
    install_vscode || return 1
}

# Execute main function
main "$@"
