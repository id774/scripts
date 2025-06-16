#!/bin/sh

########################################################################
# create_emergencyadmin.sh: Create Emergency FileVault Admin User
#
#  Description:
#  This script checks if a local admin user "emergencyadmin" exists.
#  If not, it creates the user, grants SecureToken, and adds them as a
#  FileVault-enabled user. This is intended to provide a backup unlock
#  method for encrypted systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0  2025-06-16 - Initial implementation. Wrap all logic in functions.
#                     Use interactive password input. Ensure internal sudo use.
#
#  Usage:
#  Execute the script as a normal user with administrator privileges.
#  The script will prompt for both the current admin password and the
#  password for the new emergency admin user.
#      ./create_emergencyadmin.sh
#
########################################################################

USERNAME="emergencyadmin"
FULLNAME="Emergency Admin"

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

check_user_exists() {
    if id "$USERNAME" >/dev/null 2>&1; then
        echo "[INFO] User '$USERNAME' already exists. Skipping creation."
        verify_account_status
        exit 0
    fi
}

verify_account_status() {
    echo "[INFO] Verifying account status..."

    sudo sysadminctl -secureTokenStatus "$USERNAME"
    sudo fdesetup list | grep "^$USERNAME,"
}

prompt_passwords() {
    printf "Enter password for current admin user: "
    stty -echo
    read ADMIN_PASSWORD
    stty echo
    printf "\n"

    printf "Enter password for new user '$USERNAME': "
    stty -echo
    read USER_PASSWORD
    stty echo
    printf "\n"
}

create_admin_user() {
    MAX_UID=$(dscl . -list /Users UniqueID | awk '{print $2}' | sort -n | tail -1)
    NEW_UID=$((MAX_UID + 1))

    echo "[INFO] Creating admin user '$USERNAME'..."
    sudo sysadminctl -addUser "$USERNAME" \
        -fullName "$FULLNAME" \
        -UID "$NEW_UID" \
        -password "$USER_PASSWORD" \
        -admin
}

grant_securetoken() {
    CURRENT_USER=$(logname)
    echo "[INFO] Granting SecureToken to '$USERNAME'..."
    sudo sysadminctl -adminUser "$CURRENT_USER" -adminPassword "$ADMIN_PASSWORD" \
        -secureTokenOn "$USERNAME" -password "$USER_PASSWORD"
}

add_to_filevault() {
    echo "[INFO] Adding '$USERNAME' to FileVault users..."
    echo "$USER_PASSWORD" | sudo fdesetup add -user "$USERNAME"
}

main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_sudo
    check_user_exists

    prompt_passwords
    create_admin_user
    grant_securetoken
    add_to_filevault

    echo "[INFO] Setup complete. '$USERNAME' is now a FileVault user."

    verify_account_status
    return 0
}

main
exit $?
