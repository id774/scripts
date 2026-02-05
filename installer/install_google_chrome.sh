#!/bin/sh

########################################################################
# install_google_chrome.sh: Install and Configure Google Chrome on Debian
#
#  Description:
#  This script installs Google Chrome Stable on Debian (validated on Debian 13)
#  and keeps it managed by APT. It performs the following actions:
#    1) Verify the environment (Linux, required commands, network reachability,
#       and sudo privilege).
#    2) Ensure APT infrastructure directories exist.
#    3) Fetch the latest Google Linux signing key, dearmor it to a keyring,
#       and compare its fingerprint with the currently installed keyring
#       (/usr/share/keyrings/google-linux.gpg). If missing, unreadable, or
#       fingerprint-mismatched, the keyring is (re)installed. This makes the
#       system self-heal after a key rotation or corruption.
#    4) Configure the official Google Chrome repository via a dedicated
#       sources.list.d entry with a signed-by option pointing to the keyring.
#       If a compatible line is already present, it is left as-is.
#    5) Install Google Chrome Stable only when it is not already installed.
#
#  Note:
#    This script intentionally does not run `apt update`. Ensure package
#    metadata is refreshed by your regular operations before running it.
#
#  Design goals:
#    - Idempotence: running the script multiple times converges to a stable
#      configuration without harmful side effects.
#    - Safety: key handling includes fingerprint comparison to gracefully
#      recover from key rotations.
#    - Minimal changes: no aggressive cleanup of existing repo lines; only the
#      expected configuration is added or updated when absent.
#    - Permissions: the Google Linux keyring file is always forced to mode 0644
#      after repository configuration to guarantee readability by APT. This
#      requires the availability of chmod and is safe to run repeatedly because
#      the operation is idempotent.
#
#  Requirements:
#    - Linux system with: sudo, awk, uname, cp, rm, chmod, mktemp, curl, gpg, tee, apt, grep, mkdir, dpkg
#    - Network connectivity to dl.google.com
#    - Sudo privilege for system modifications
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_google_chrome.sh
#
#  Version History:
#  v1.1 2026-02-05
#       Remove preflight network connectivity check.
#  v1.0 2025-09-04
#       Initial release for Debian 13.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
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
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Prepare filesystem locations required for APT keyrings and source entries.
# This function validates the overall environment by running a series of checks
# (system type, required commands, network connectivity, sudo privileges) and
# ensures that the directories used for APT keyrings and source list entries
# exist. All operations are idempotent: re-running the function will not cause
# duplication or harm.
setup_environment() {
    echo "[INFO] Validating environment..."

    # Confirm we are on a Linux system.
    check_system

    echo "[INFO] Checking required commands..."
    # Verify that all external tools required by the script are available.
    check_commands awk uname cp rm chmod mktemp curl gpg tee apt grep mkdir dpkg

    # Verify the current user has sudo privileges for system modifications.
    check_sudo

    # Ensure the keyrings directory exists.
    # If it does not exist, create it. If creation fails, abort.
    if [ ! -d /usr/share/keyrings ]; then
        if ! sudo mkdir -p /usr/share/keyrings; then
            echo "[ERROR] Failed to create /usr/share/keyrings" >&2
            exit 1
        fi
    fi

    # Ensure the sources.list.d directory exists for custom repository entries.
    # If it does not exist, create it. If creation fails, abort.
    if [ ! -d /etc/apt/sources.list.d ]; then
        if ! sudo mkdir -p /etc/apt/sources.list.d; then
            echo "[ERROR] Failed to create /etc/apt/sources.list.d" >&2
            exit 1
        fi
    fi
}

# Perform key acquisition with fingerprint comparison for self-healing behavior.
# The latest public key is fetched, dearmored to a temporary keyring, and its
# fingerprint is compared to the installed keyring. Differences trigger a
# replacement; missing or unreadable keyrings are also replaced.
install_or_update_keyring() {
    KEYRING="/usr/share/keyrings/google-linux.gpg"

    # Create temporary files for the fetched public key and its dearmored keyring.
    TMP_KEY_PUB="$(mktemp)"
    TMP_KEY_RING="$(mktemp)"

    echo "[INFO] Fetching Google Linux signing key..."
    # Fetch the current public key from Google; fail fast if unreachable.
    if ! curl -fsSL https://dl.google.com/linux/linux_signing_key.pub > "$TMP_KEY_PUB"; then
        echo "[ERROR] Failed to fetch Google signing key." >&2
        rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
        exit 1
    fi

    # Convert the armored public key into a binary keyring format for APT.
    if ! gpg --dearmor < "$TMP_KEY_PUB" > "$TMP_KEY_RING" 2>/dev/null; then
        echo "[ERROR] Failed to dearmor fetched Google key." >&2
        rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
        exit 1
    fi

    # Read the fingerprint of the newly fetched key (the "expected" state).
    NEW_FPR="$(gpg --show-keys --with-colons "$TMP_KEY_RING" 2>/dev/null | awk -F: '/^fpr:/ {print $10; exit}')"
    if [ -z "$NEW_FPR" ]; then
        # If we cannot parse a fingerprint, the fetched key is unusable; abort.
        echo "[ERROR] Failed to parse fingerprint from fetched Google key." >&2
        rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
        exit 1
    fi

    # If a keyring is already installed, compare fingerprints to decide action.
    if [ -f "$KEYRING" ]; then
        # Obtain the fingerprint of the currently installed keyring.
        CUR_FPR="$(gpg --show-keys --with-colons "$KEYRING" 2>/dev/null | awk -F: '/^fpr:/ {print $10; exit}')"

        if [ -z "$CUR_FPR" ]; then
            # Branch A: The existing keyring is present but unreadable (corrupted or invalid).
            # Action: Replace it with the freshly fetched key (self-heal).
            echo "[WARN] Existing keyring is unreadable; replacing with the fresh key."
            if ! sudo cp "$TMP_KEY_RING" "$KEYRING"; then
                echo "[ERROR] Failed to update keyring $KEYRING" >&2
                rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
                exit 1
            fi
            echo "[INFO] Replaced unreadable keyring. Fingerprint: $NEW_FPR"

        elif [ "$CUR_FPR" != "$NEW_FPR" ]; then
            # Branch B: The installed keyring has a different fingerprint (key rotation detected).
            # Action: Update the keyring to the new key so APT continues to trust Google packages.
            echo "[INFO] Google signing key has changed. Updating keyring."
            if ! sudo cp "$TMP_KEY_RING" "$KEYRING"; then
                echo "[ERROR] Failed to update keyring $KEYRING" >&2
                rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
                exit 1
            fi
            echo "[INFO] Keyring updated. New fingerprint: $NEW_FPR"

        else
            # Branch C: Fingerprints match; the keyring is already up-to-date.
            # Action: No changes needed.
            echo "[INFO] Keyring is up-to-date. Fingerprint: $CUR_FPR"
        fi

    else
        # Branch D: No keyring exists yet.
        # Action: Install the freshly fetched keyring to enable package verification.
        if ! sudo cp "$TMP_KEY_RING" "$KEYRING"; then
            echo "[ERROR] Failed to install keyring $KEYRING" >&2
            rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
            exit 1
        fi
        echo "[INFO] Installed keyring. Fingerprint: $NEW_FPR"
    fi

    # Always clean temporary files to avoid leaking sensitive material.
    rm -f "$TMP_KEY_PUB" "$TMP_KEY_RING"
}

# Ensure the Google Chrome APT source entry exists and points at the keyring.
# If the expected line is missing, it is created or updated accordingly.
configure_repository() {
    KEYRING="/usr/share/keyrings/google-linux.gpg"
    LISTFILE="/etc/apt/sources.list.d/google-chrome.list"
    REPO_LINE="deb [arch=amd64 signed-by=${KEYRING}] http://dl.google.com/linux/chrome/deb/ stable main"

    echo "[INFO] Configuring APT repository for Google Chrome..."

    # Case 1: No repo file exists -> create a fresh one with the expected line.
    if [ ! -f "$LISTFILE" ]; then
        echo "$REPO_LINE" | sudo tee "$LISTFILE" >/dev/null
        echo "[INFO] Created repo file: $LISTFILE"

    else
        # Case 2: Repo file exists. Check if the expected line is already present.
        if ! grep -q "^deb \\[arch=amd64 signed-by=${KEYRING}\\] http://dl.google.com/linux/chrome/deb/ stable main\$" "$LISTFILE"; then
            # Branch 2A: The expected line is missing or differs (e.g., no signed-by).
            # Action: Write the canonical line so that APT uses the dedicated keyring.
            echo "$REPO_LINE" | sudo tee "$LISTFILE" >/dev/null
            echo "[INFO] Updated repo file: $LISTFILE"
        else
            # Branch 2B: The expected line already exists and matches exactly.
            # Action: Leave the file as-is (no destructive normalization performed).
            echo "[INFO] Repo already configured: $LISTFILE"
        fi
    fi
    sudo chmod 0644 "$KEYRING"
}

# Ensure the package is installed while keeping re-runs quiet.
# This function assumes APT metadata is already up to date (no `apt update`
# here). It checks whether google-chrome-stable is installed and installs it
# only when absent to maintain idempotence and reduce noise.
install_package_if_needed() {
    # apt metadata refresh is intentionally omitted by design.
    # If installation fails due to stale metadata, run `sudo apt update` outside
    # this script as part of your regular maintenance flow.

    # Check if the Chrome package is already installed.
    if dpkg -s google-chrome-stable >/dev/null 2>&1; then
        # Branch A: Package is already present.
        # Action: Skip installation to avoid redundant work.
        echo "[INFO] google-chrome-stable is already installed. Skipping installation."
    else
        # Branch B: Package is not installed yet.
        # Action: Proceed with installation through APT.
        echo "[INFO] Installing google-chrome-stable..."
        if ! sudo apt install -y google-chrome-stable; then
            echo "[ERROR] Failed to install google-chrome-stable." >&2
            exit 1
        fi
        echo "[INFO] Google Chrome installation completed."
    fi
}

# Orchestrate the overall installation flow.
install_google_chrome() {
    setup_environment
    install_or_update_keyring
    configure_repository
    install_package_if_needed
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    install_google_chrome "$@"
    return 0
}

# Execute main function
main "$@"
