#!/bin/sh

########################################################################
# install_vscode.sh: Install Visual Studio Code on Debian/Ubuntu
#
#  Description:
#  Install Visual Studio Code Stable from Microsoft's official APT repository.
#  Use APT to identify existing repositories instead of parsing APT source files.
#  Install the signing key and tracked source only when absent, then install code.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_vscode.sh
#      ./install_vscode.sh --insecure-tls
#
#  Options:
#  --insecure-tls: Persistently disable APT TLS certificate checks for the
#                  vendor host on Debian/Ubuntu and use curl -k for key download.
#
#  Requirements:
#  - Debian or Ubuntu based Linux (amd64, arm64, or armhf).
#  - Sudo privileges.
#  - Repository template: ../etc/apt.conf.d/sources-vscode.sources.
#  - Network connectivity when the key or package must be installed.
#
#  Exit Status:
#  0: Success or help/version display.
#  1: Unsupported environment, conflicting configuration, or install failure.
#  126: Required command exists but is not executable.
#  127: Required command is missing.
#
#  Notes:
#  - This script does not overwrite existing APT sources or signing keys.
#  - The VS Code package may manage its own APT source during installation.
#  - An installed code package is left to normal APT upgrades.
#  - --insecure-tls installs a persistent, host-specific APT configuration;
#    later APT runs remain insecure until the configuration is removed.
#  - This bypass weakens TLS authentication; prefer trusting the organization's
#    inspection CA and remove the bypass when it is no longer needed.
#  - The configuration is /etc/apt/apt.conf.d/98-insecure-tls-microsoft.conf
#    for packages.microsoft.com. It applies to every later APT operation for
#    that host, and remains when the installation succeeds, fails, or is
#    skipped because VS Code is already installed.
#  - Omitting --insecure-tls later does not remove the configuration. Remove
#    it manually with: sudo rm /etc/apt/apt.conf.d/98-insecure-tls-microsoft.conf
#  - Package signature and signing-key fingerprint checks stay enabled, but
#    they do not restore TLS server authentication.
#  - A differing existing configuration file is never overwritten; other APT
#    configuration may override this setting.
#
#  Version History:
#  v1.1 2026-10-16
#       Add opt-in persistent APT TLS verification bypass for inspected networks.
#  v1.0 2026-10-13
#       Initial release.
#
########################################################################

# Display the structured header
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

# Check the operating system and architecture
check_system() {
    if [ "$(uname -s 2>/dev/null)" != Linux ] || [ ! -r /etc/os-release ]; then
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

    architecture=$(dpkg --print-architecture) || return 1
    case "$architecture" in
        amd64|arm64|armhf) ;;
        *)
            echo "[ERROR] Unsupported Debian package architecture." >&2
            return 1
            ;;
    esac
}

# Check sudo privileges
check_sudo() {
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        return 1
    fi
}

# Resolve installation paths
setup_paths() {
    script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd) || return 1
    source_template="$script_dir/../etc/apt.conf.d/sources-vscode.sources"
    tls_template="$script_dir/../etc/apt.conf.d/98-insecure-tls-microsoft.conf"
    tls_target=/etc/apt/apt.conf.d/98-insecure-tls-microsoft.conf
    tls_host=packages.microsoft.com
    source_target=/etc/apt/sources.list.d/vscode.sources
    keyring=/usr/share/keyrings/microsoft.gpg
    repository_base=https://packages.microsoft.com/repos/code/dists/stable
    expected_fingerprint=BC528686B50D79E339D3721CEB3E94ADBE1229CF

    if [ ! -r "$source_template" ]; then
        echo "[ERROR] Missing repository template: $source_template" >&2
        return 1
    fi
}

# Return the fingerprint only when exactly one primary key exists
key_fingerprint() {
    gpg --batch --show-keys --with-colons "$1" 2>/dev/null |
        awk -F: '
            $1 == "pub" || $1 == "sec" { count++; want = 1; next }
            $1 == "sub" || $1 == "ssb" { want = 0; next }
            $1 == "fpr" && want { fingerprint = $10; want = 0 }
            END { if (count == 1) print fingerprint }
        '
}

# Persistently disable APT TLS verification for the vendor host on request
configure_insecure_tls() {
    check_commands cmp
    if [ ! -r "$tls_template" ]; then
        echo "[ERROR] Missing TLS configuration template: $tls_template" >&2
        return 1
    fi

    tls_present=false
    if [ -e "$tls_target" ] || [ -L "$tls_target" ]; then
        if [ -L "$tls_target" ] || [ ! -f "$tls_target" ] ||
           [ ! -r "$tls_target" ] || ! cmp -s "$tls_template" "$tls_target"; then
            echo "[ERROR] Existing APT TLS configuration differs: $tls_target" >&2
            return 1
        fi
        tls_present=true
    fi

    echo "[WARN] Persistently disabling APT TLS certificate checks for $tls_host in $tls_target. Remove the file manually to restore verification." >&2
    if [ "$tls_present" = true ]; then
        echo "[INFO] APT TLS configuration is already installed."
        return 0
    fi

    if ! sudo mkdir -p /etc/apt/apt.conf.d ||
       ! sudo install -o root -g root -m 0644 \
        "$tls_template" "$tls_target"; then
        echo "[ERROR] Failed to install APT TLS configuration." >&2
        return 1
    fi
    echo "[INFO] Installed APT TLS configuration."
}

# Install the verified Microsoft signing key when absent
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

    curl_extra_opts=
    if [ "$insecure_tls" = true ]; then
        curl_extra_opts=-k
    fi

    echo "[INFO] Downloading Microsoft signing key..."
    if ! curl -fsSL $curl_extra_opts https://packages.microsoft.com/keys/microsoft.asc \
        -o "$temp_dir/microsoft.asc" ||
       ! gpg --batch --dearmor < "$temp_dir/microsoft.asc" \
        > "$temp_dir/microsoft.gpg"; then
        echo "[ERROR] Failed to download or convert Microsoft signing key." >&2
        return 1
    fi

    if [ "$(key_fingerprint "$temp_dir/microsoft.gpg")" != "$expected_fingerprint" ]; then
        echo "[ERROR] Unexpected Microsoft signing key fingerprint." >&2
        return 1
    fi

    if ! sudo mkdir -p /usr/share/keyrings ||
       ! sudo install -o root -g root -m 0644 \
        "$temp_dir/microsoft.gpg" "$keyring"; then
        echo "[ERROR] Failed to install Microsoft signing key." >&2
        return 1
    fi

    echo "[INFO] Installed Microsoft signing key."
}

# Let APT interpret its own source files instead of parsing them with awk
configure_repository() {
    if [ -e "$source_target" ] || [ -L "$source_target" ]; then
        if [ ! -f "$source_target" ] ||
           [ -L "$source_target" ] ||
           [ ! -r "$source_target" ]; then
            echo "[ERROR] Invalid existing APT source: $source_target" >&2
            return 1
        fi
    fi

    uris=$(apt-get --print-uris update) || {
        echo "[ERROR] Failed to inspect APT sources." >&2
        return 1
    }

    case "$uris" in
        *"'$repository_base/main/binary-$architecture/Packages"*)
            echo "[INFO] VS Code APT repository is already configured."
            return 0
            ;;
        *"'$repository_base/InRelease'"*)
            echo "[ERROR] Existing VS Code APT source does not provide $architecture packages." >&2
            return 1
            ;;
    esac

    if [ -e "$source_target" ]; then
        echo "[ERROR] Existing APT source differs: $source_target" >&2
        return 1
    fi

    if ! sudo mkdir -p /etc/apt/sources.list.d ||
       ! sudo install -o root -g root -m 0644 \
        "$source_template" "$source_target"; then
        echo "[ERROR] Failed to install VS Code APT repository configuration." >&2
        return 1
    fi

    echo "[INFO] Configured VS Code APT repository."
}

# Install VS Code only when the package is absent
install_vscode() {
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

# Main entry point
main() {
    insecure_tls=false
    case "$1" in
        -h|--help|-v|--version) usage ;;
        "") [ "$#" -eq 0 ] || usage ;;
        --insecure-tls) [ "$#" -eq 1 ] || usage; insecure_tls=true ;;
        *) usage ;;
    esac

    check_commands uname dpkg dpkg-query apt-get gpg awk grep \
                   install mkdir mktemp rm dirname

    check_system || return 1
    check_sudo || return 1
    setup_paths || return 1
    if [ "$insecure_tls" = true ]; then
        configure_insecure_tls || return 1
    fi
    if dpkg-query -W -f='${Status}' code 2>/dev/null |
       grep -q '^install ok installed$'; then
        echo "[INFO] Visual Studio Code is already installed."
        return 0
    fi
    install_signing_key || return 1
    configure_repository || return 1
    install_vscode || return 1
}

main "$@"
