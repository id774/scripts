#!/bin/sh

########################################################################
# setup_scripts.sh: Setup Script for Scripts Collection
#
#  Description:
#  This script sets permissions for a collection of scripts.
#  - Grants read/write permissions to the owner, and read-only to group and others.
#  - Grants execute permissions to all files with .sh, .py, or .rb extensions under the entire collection.
#  - Grants execute permissions to all files under scripts/cron/bin regardless of extension.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_scripts.sh
#
#  Run this script to set up the permissions for a collection of scripts.
#  Ensure that the SCRIPTS environment variable is set to the path of
#  your script collection before running this script.
#
#  Notes:
#  - This script should be run from the root directory of the script collection.
#  - Make sure to back up your scripts before running this script as a precaution.
#  - SCRIPTS environment variable must be set to the path of the script collection.
#  - Execute permissions will be added:
#      - To all *.sh, *.py, *.rb files under the SCRIPTS path
#      - To all *.sh, *.py, *.rb files in the current directory
#      - To all files under scripts/cron/bin (no extension filter)
#
#  Version History:
#  v2.3 2025-12-15
#       Skip missing directories (installer, test, cron/bin) without failing to keep idempotent behavior.
#  v2.2 2025-08-31
#       Fix include current directory in execute permission handling for *.sh, *.py, *.rb.
#  v2.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#       Add execute permission handling for test directory scripts with .sh, .py, or .rb extensions.
#  v2.0 2025-06-16
#       Add execute permission handling for all files under scripts/installer.
#  v1.9 2025-05-12
#       Replace chmod -R with find-based permission control for finer safety.
#       Grant execute permissions to all files under scripts/cron/bin regardless of extension.
#  v1.8 2025-04-25
#       Add INFO log before each permission operation to improve clarity.
#       Verify all return codes to ensure successful completion before final message.
#       Output [ERROR] message if any permission operation fails.
#  v1.7 2025-04-12
#       Revoke execute permissions from scripts/cron/bin to avoid
#       accidental manual execution of cron job scripts.
#  v1.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.5 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.2 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.1 2023-12-08
#       Added documentation and environment variable check for SCRIPTS.
#  v1.0 2008-08-22
#       Initial release.
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

# Check if the SCRIPTS variable is unset or empty
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Set file permissions
set_permissions() {
    echo "[INFO] Setting read/write permissions for all script files."
    find "$SCRIPTS" -type f -exec chmod u+rw,g+r,g-w,o+r,o-w {} \;
    RC1=$?

    echo "[INFO] Granting execute permissions to script files (*.sh, *.py, *.rb) including current directory."
    find "$SCRIPTS" -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;
    find . -maxdepth 1 -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;
    RC2=$?

    echo "[INFO] Granting execute permissions to installer scripts (*.sh, *.py, *.rb)."
    RC3=0
    if [ -d "$SCRIPTS/installer" ]; then
        find "$SCRIPTS/installer" -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;
        RC3=$?
    else
        echo "[INFO] installer directory not found; skipping."
    fi

    echo "[INFO] Granting execute permissions to test scripts (*.sh, *.py, *.rb)."
    RC4=0
    if [ -d "$SCRIPTS/test" ]; then
        find "$SCRIPTS/test" -type f \( -name "*.sh" -o -name "*.py" -o -name "*.rb" \) -exec chmod u+x,g+x,o+x {} \;
        RC4=$?
    else
        echo "[INFO] test directory not found; skipping."
    fi

    echo "[INFO] Granting execute permissions to all files under scripts/cron/bin."
    RC5=0
    if [ -d "$SCRIPTS/cron/bin" ]; then
        find "$SCRIPTS/cron/bin" -type f -exec chmod u+x,g+x,o+x {} \;
        RC5=$?
    else
        echo "[INFO] cron/bin directory not found; skipping."
    fi

    if [ "$RC1" -eq 0 ] && [ "$RC2" -eq 0 ] && [ "$RC3" -eq 0 ] && [ "$RC4" -eq 0 ] && [ "$RC5" -eq 0 ]; then
        echo "[INFO] All permission settings completed successfully."
    else
        echo "[ERROR] One or more permission operations failed." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_scripts
    check_commands chmod find
    set_permissions
    return 0
}

# Execute main function
main "$@"
