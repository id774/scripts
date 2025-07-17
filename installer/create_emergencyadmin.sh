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
#  Usage:
#  Execute the script as a normal user with administrator privileges.
#  The script will prompt for both the current admin password and the
#  password for the new emergency admin user.
#      ./create_emergencyadmin.sh
#
#  Version History:
#  v1.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.0 2025-06-16
#       First version.
#
########################################################################

USERNAME="emergencyadmin"
FULLNAME="Emergency Admin"

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is macOS
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

# Check if the specified user already exists and verify status if so
check_user_exists() {
    if id "$USERNAME" >/dev/null 2>&1; then
        echo "[INFO] User '$USERNAME' already exists. Skipping creation."
        verify_account_status
        exit 0
    fi
}

# Set the home directory permission to 700 for emergencyadmin and current user
set_home_permission() {
    echo "[INFO] Setting permission 700 for /Users/$USERNAME"
    sudo chmod 700 "/Users/$USERNAME"
    ls -ld "/Users/$USERNAME"

    CURRENT_USER=$(logname)
    CURRENT_HOME=$(dscl . -read /Users/"$CURRENT_USER" NFSHomeDirectory | awk '{print $2}')

    echo "[INFO] Setting permission 700 for current user home: $CURRENT_HOME"
    sudo chmod 700 "$CURRENT_HOME"
    ls -ld "$CURRENT_HOME"
}

# Verify the account's SecureToken, FileVault status, and login window visibility
verify_account_status() {
    set_home_permission

    echo "[INFO] Verifying account status..."

    sudo sysadminctl -secureTokenStatus "$USERNAME"
    sudo fdesetup list

    echo "[INFO] Current HiddenUsersList:"
    sudo defaults read /Library/Preferences/com.apple.loginwindow HiddenUsersList
}

# Prompt for admin and new user passwords with hidden input
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

# Create a new admin user with a unique UID
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

# Grant SecureToken to the newly created user
grant_securetoken() {
    CURRENT_USER=$(logname)
    echo "[INFO] Granting SecureToken to '$USERNAME'..."
    sudo sysadminctl -adminUser "$CURRENT_USER" -adminPassword "$ADMIN_PASSWORD" \
        -secureTokenOn "$USERNAME" -password "$USER_PASSWORD"
}

# Add the new user to FileVault authorized users
add_to_filevault() {
    echo "[INFO] Adding '$USERNAME' to FileVault users..."
    echo "$USER_PASSWORD" | sudo fdesetup add -user "$USERNAME"
}

# Hide the new user from the macOS login window
hide_user_from_loginwindow() {
    echo "[INFO] Hiding '$USERNAME' from login window..."
    sudo defaults write /Library/Preferences/com.apple.loginwindow HiddenUsersList -array-add "$USERNAME"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_sudo
    check_user_exists

    prompt_passwords
    create_admin_user
    grant_securetoken
    add_to_filevault
    hide_user_from_loginwindow

    echo "[INFO] Setup complete. '$USERNAME' is now a FileVault user."

    verify_account_status
    return 0
}

# Execute main function
main "$@"
exit $?
