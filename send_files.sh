#!/bin/sh

########################################################################
# send_files.sh: Compress a directory with password and send via Gmail
#
#  Description:
#  This script compresses a specified source directory into a password-
#  protected ZIP archive, generates a secure random password, and saves both
#  to a temporary directory. By default, the archive is copied to a configured
#  output directory. Optionally, the archive can be emailed via Gmail using
#  standard POSIX tools (`mail`, `uuencode`) if -s or --send is specified.
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
#  Usage:
#      ./send_files.sh [-s|--send] [-z|--7z] [-d|--dir <directory>]
#      This script is intended to be executed manually or via cron.
#      Without -s, the archive is saved to a local directory.
#      With -s, the archive is sent as an email attachment (legacy behavior).
#      If -z is specified, the archive is created using 7z (AES-256 encrypted).
#      Otherwise, ZIP is used by default.
#      If -d is specified, overrides the SOURCE_DIR from config.
#
#  Features:
#  - Compress a target directory into a password-protected ZIP.
#  - Optionally use 7z format with AES-256 encryption (via -z).
#  - Override archive source directory via -d <dir> option.
#  - Generate a secure random password (configurable length).
#  - Save password to a local text file (configurable name).
#  - Email the archive as attachment to a configured Gmail address.
#  - POSIX-compliant and portable across Unix-like systems.
#
#  Notes:
#  - Ensure `mail` and `uuencode` are installed and properly configured.
#  - Gmail SMTP setup may be required (via mailx configuration).
#  - No password is sent in the email — it is stored locally only.
#  - To use 7z, the `7z` command must be available in PATH.
#
#  Error Conditions:
#  1. General failure (e.g., password generation or ZIP failure).
#  2. Configuration file not found.
#  3. Source directory does not exist.
#  4. GMAIL_TO address is not set.
#  5. ARCHIVE_OUTPUT_DIR is not set in config.
#  6. Archive output directory does not exist.
#  8. Unsafe operation: sending .7z archive via email without user confirmation.
#  9. Failed to send mail.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-05-23
#       Show generated password after archive creation.
#       Display download URL when storing archive locally using DOWNLOAD_BASE_URL.
#  v1.3 2025-05-09
#       Add support for -z|--7z option to create 7z archive instead of ZIP.
#       Uses AES-256 encryption and full-directory protection via 7z.
#       Maintains full backward compatibility with default ZIP behavior.
#       Added safety confirmation when both -s and -z options are specified.
#       Prevents accidental Gmail rejection of .7z attachments unless explicitly approved.
#       Add -d|--dir option to override SOURCE_DIR at runtime.
#       Enables flexible archive input without modifying config file.
#       Ensures option validation and proper integration with existing modes.
#  v1.2 2025-05-08
#       Add support for ARCHIVE_OUTPUT_DIR from external config.
#       Introduced -s|--send option to enable legacy email behavior.
#       Default behavior is now to save the archive to a configured directory.
#  v1.1 2025-05-07
#       Add ARCHIVE_FILE_NAME support to load archive filename base from external config.
#       This allows custom naming of output ZIP files using timestamp suffix.
#  v1.0 2025-05-05
#       Initial release. Implemented archive, password generation,
#       and email delivery using external configuration.
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

# Validate configuration based on mode
check_environment() {
    if [ "$USE_7Z" = "yes" ]; then
        check_commands 7z
        ARCHIVE_EXT=".7z"
    else
        check_commands zip
        ARCHIVE_EXT=".zip"
    fi

    if [ ! -d "$SOURCE_DIR" ]; then
        echo "[ERROR] Directory not found: $SOURCE_DIR" >&2
        exit 3
    fi

    if [ "$SEND_MODE" = "yes" ]; then
        # Mail send mode.
        check_commands uuencode mail head basename dirname date

        if [ -z "$GMAIL_TO" ]; then
            echo "[ERROR] GMAIL_TO address is not set." >&2
            exit 4
        fi
    else
        # File store mode.
        check_commands head basename dirname date cp

        if [ -z "$ARCHIVE_OUTPUT_DIR" ]; then
            echo "[ERROR] ARCHIVE_OUTPUT_DIR is not set in config." >&2
            exit 5
        elif [ ! -d "$ARCHIVE_OUTPUT_DIR" ]; then
            echo "[ERROR] Archive output directory does not exist: $ARCHIVE_OUTPUT_DIR" >&2
            exit 6
        fi
    fi
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
        exit 2
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

create_archive() {
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)

    if [ -z "$ARCHIVE_FILE_NAME" ]; then
        echo "[WARN] ARCHIVE_FILE_NAME is not set; using default: secure_archive$ARCHIVE_EXT" >&2
    fi

    ARCHIVE_BASENAME="${ARCHIVE_FILE_NAME:-secure_archive}"
    ZIP_PATH="$TMP/${ARCHIVE_BASENAME}_${TIMESTAMP}${ARCHIVE_EXT}"
    PASSWORD=$(generate_password)

    PASSWORD_FILE="${TMP}/${PASSWORD_FILE_NAME%.txt}_${TIMESTAMP}.txt"
    echo "$PASSWORD" > "$PASSWORD_FILE"
    echo "[INFO] Generated password: $PASSWORD"

    cd "$(dirname "$SOURCE_DIR")" || exit 1
    if [ "$USE_7Z" = "yes" ]; then
        7z a -p"$PASSWORD" -mhe=on "$ZIP_PATH" "$(basename "$SOURCE_DIR")" > /dev/null 2>&1
        RC=$?
    else
        zip -r -P "$PASSWORD" "$ZIP_PATH" "$(basename "$SOURCE_DIR")" > /dev/null 2>&1
        RC=$?
    fi

    if [ "$RC" -eq 0 ]; then
        echo "[INFO] Archive successfully created: $ZIP_PATH"
    else
        echo "[ERROR] Failed to create archive (exit code: $RC)" >&2
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

# Output directory for archive (if not sending)
store_archive() {
    cp "$ZIP_PATH" "$ARCHIVE_OUTPUT_DIR"
    echo "[INFO] Archive copied to $ARCHIVE_OUTPUT_DIR"

    if [ -n "$DOWNLOAD_BASE_URL" ]; then
        echo "[INFO] Download URL: ${DOWNLOAD_BASE_URL}/$(basename "$ZIP_PATH")"
    fi
}

confirm_send_7z() {
    echo "[WARN] You have specified both --send and --7z options."
    echo
    echo "Sending .7z archives via Gmail is strongly discouraged due to the following risks:"
    echo "- Gmail often blocks .7z attachments even if they are harmless."
    echo "- This is because 7z files can contain encrypted content that cannot be malware-scanned."
    echo "- Your email may be rejected with a 552 5.7.0 error or silently discarded."
    echo
    echo "Do you still want to proceed? Type 'yes' or 'y' to continue:"
    read REPLY
    case "$REPLY" in
        y|yes) echo "[INFO] Proceeding despite risk." ;;
        *) echo "[ERROR] Aborted due to security risk of sending .7z file via Gmail." >&2; exit 8 ;;
    esac
}

# Main entry point of the script
main() {
    SEND_MODE="no"
    USE_7Z="no"
    OVERRIDE_SOURCE_DIR=""

    while [ $# -gt 0 ]; do
        case "$1" in
            -h|--help|-v|--version) usage ;;
            -s|--send) SEND_MODE="yes" ;;
            -z|--7z)   USE_7Z="yes" ;;
            -d|--dir)
                shift
                if [ -z "$1" ]; then
                    echo "[ERROR] Missing argument for -d|--dir" >&2
                    exit 1
                fi
                OVERRIDE_SOURCE_DIR="$1"
                ;;
            *) echo "[ERROR] Unknown option: $1" >&2; exit 1 ;;
        esac
        shift
    done

    load_config

    if [ -n "$OVERRIDE_SOURCE_DIR" ]; then
        SOURCE_DIR="$OVERRIDE_SOURCE_DIR"
    fi

    check_environment

    if [ "$SEND_MODE" = "yes" ] && [ "$USE_7Z" = "yes" ]; then
        confirm_send_7z
    fi

    create_archive

    if [ "$SEND_MODE" = "yes" ]; then
        send_mail
    else
        store_archive
    fi
    return 0
}

# Execute main function
main "$@"
exit $?
