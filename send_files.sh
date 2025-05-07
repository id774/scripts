#!/bin/sh

########################################################################
# send_files.sh: Compress a directory with password and send via Gmail
#
#  Description:
#  This script compresses a specified source directory into a password-
#  protected ZIP archive, stores the archive in a configurable temporary
#  directory, generates a secure random password, and saves it to a
#  specified password file. The archive is then sent via Gmail using
#  standard POSIX tools (`mail`, `uuencode`).
#
#  The configuration values such as temporary directory, target email
#  address, archive source, archive password file name, and password
#  length are loaded from an external config file.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-05-07
#       Add ARCHIVE_FILE_NAME support to load archive filename base from external config.
#       This allows custom naming of output ZIP files using timestamp suffix.
#  v1.0 2025-05-05
#       Initial release. Implemented archive, password generation,
#       and email delivery using external configuration.
#
#  Usage:
#      ./send_files.sh
#      This script is intended to be executed manually or via cron.
#
#  Features:
#  - Compress a target directory into a password-protected ZIP
#  - Generate a secure random password (configurable length)
#  - Save password to a local text file (configurable name)
#  - Email the archive as attachment to a configured Gmail address
#  - POSIX-compliant and portable across Unix-like systems
#
#  Notes:
#  - Ensure `mail` and `uuencode` are installed and properly configured
#  - Gmail SMTP setup may be required (via mailx configuration)
#  - No password is sent in the email — it is stored locally only
#
#  Error Conditions:
#  2. Source directory does not exist.
#  3. Configuration file not found.
#  9. Failed to send mail.
#
########################################################################

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

# Check if BASE_DIR exists
check_environment() {
    if [ -z "$GMAIL_TO" ]; then
        echo "[ERROR] GMAIL_TO address is not set." >&2
        exit 1
    fi

    if [ ! -d "$SOURCE_DIR" ]; then
        echo "[ERROR] Directory not found: $SOURCE_DIR" >&2
        exit 2
    fi
}

# Function to check required commands
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

# Load configuration from external file
load_config() {
    SCRIPT_DIR=$(dirname "$0")
    CONFIG_FILE="$SCRIPT_DIR/etc/send_files.conf"

    if [ ! -f "$CONFIG_FILE" ]; then
        CONFIG_FILE="$SCRIPT_DIR/../etc/send_files.conf"
    fi

    if [ -f "$CONFIG_FILE" ]; then
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE" >&2
        exit 3
    fi

    # Set defaults if not configured
    : "${TMP:=/tmp}"
    : "${PASSWORD_LENGTH:=20}"
    : "${MAIL_SUBJECT:=[admin] Server Files}"
    : "${PASSWORD_FILE_NAME:=zip_password.txt}"
}

# Generate a random password with mixed characters
generate_password() {
    if [ "$(uname)" = "Darwin" ] && command -v gtr >/dev/null 2>&1; then
        gtr -dc 'A-Za-z0-9!@#$%^&*()_+=-' < /dev/urandom | head -c "$PASSWORD_LENGTH"
    else
        check_commands tr
        tr -dc 'A-Za-z0-9!@#$%^&*()_+=-' < /dev/urandom | head -c "$PASSWORD_LENGTH"
    fi
}

create_zip() {
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)

    if [ -z "$ARCHIVE_FILE_NAME" ]; then
        echo "[WARN] ARCHIVE_FILE_NAME is not set; using default: secure_archive.zip" >&2
    fi

    ARCHIVE_BASENAME="${ARCHIVE_FILE_NAME:-secure_archive}"
    ZIP_PATH="$TMP/${ARCHIVE_BASENAME}_${TIMESTAMP}.zip"
    PASSWORD=$(generate_password)

    echo "$PASSWORD" > "$TMP/$PASSWORD_FILE_NAME"

    cd "$(dirname "$SOURCE_DIR")" || exit 1
    zip -r -P "$PASSWORD" "$ZIP_PATH" "$(basename "$SOURCE_DIR")" > /dev/null 2>&1
    RC=$?

    if [ "$RC" -eq 0 ]; then
        echo "[INFO] zip archive successfully created: $ZIP_PATH"
    else
        echo "[ERROR] Failed to create zip archive (exit code: $RC)" >&2
        exit "$RC"
    fi
}

# Send the archive via email using mailx
send_mail() {
    SUBJECT="$MAIL_SUBJECT"
    BODY="The password-protected archive is attached.
Password is stored locally in $TMP/$PASSWORD_FILE_NAME"

    # Compose and send mail
    echo "$BODY" > "$TMP/mail_body.txt"
    uuencode "$ZIP_PATH" "$(basename "$ZIP_PATH")" > "$TMP/mail_attachment.txt"

    if [ ! -s "$TMP/mail_attachment.txt" ]; then
        echo "[ERROR] Failed to encode attachment." >&2
        exit 1
    fi

    cat "$TMP/mail_body.txt" "$TMP/mail_attachment.txt" | mail -s "$SUBJECT" "$GMAIL_TO"
    RC=$?

    if [ "$RC" -eq 0 ]; then
        echo "[INFO] Mail successfully sent to $GMAIL_TO"
    else
        echo "[ERROR] Failed to send mail to $GMAIL_TO (exit code: $RC)" >&2
        exit 9
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_commands zip uuencode mail head basename dirname date
    load_config
    check_environment

    create_zip
    send_mail
}

# Execute main function
main "$@"
