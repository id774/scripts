#!/bin/sh

########################################################################
# install_chkrootkit.sh: chkrootkit Setup Script
#
#  Description:
#  Set up automated rootkit detection using chkrootkit by deploying the cron job
#  and log rotation. Ensure required directories and log files exist, set proper
#  permissions, and register periodic scans. On Debian style environments that
#  provide /etc/cron.daily/chkrootkit with the log.expected baseline flow, run
#  an initial scan and create /var/log/chkrootkit/log.expected from log.today
#  if it does not exist, so that only future diffs trigger alerts.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_chkrootkit.sh
#      ./install_chkrootkit.sh --uninstall
#
#  Notes:
#  - Ensure SCRIPTS is set to the directory containing the chkrootkit assets
#    before running this script.
#  - Baseline initialization relies on the Debian style wrapper at
#    /etc/cron.daily/chkrootkit that supports the log.expected mechanism.
#    On non Debian style systems or older packages, baseline initialization
#    becomes a no op and the installation still succeeds.
#  - Creating log.expected trusts the current log.today as baseline. Review
#    the first run output before accepting it as expected in security critical
#    environments.
#
#  Version History:
#  v2.1 2025-09-04
#       Add Debian style baseline initialization using log.expected.
#  v2.0 2025-08-01
#       Add --uninstall option and refactor install into separate function.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-27
#       Simplified error checking to critical filesystem operations.
#  v1.4 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-14
#       Added command existence check, system check, and environment variable validation.
#  v1.0 2012-05-15
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the chkrootkit script." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Initialize chkrootkit baseline on Debian style environments
# - Detect /etc/cron.daily/chkrootkit and confirm it supports log.expected.
# - If log.expected does not exist, execute the daily wrapper once to generate
#   log.today (if needed), then copy log.today to establish the baseline.
#   This suppresses recurring benign alerts while preserving diffs for future runs.
# - No effect on non Debian style or older environments; safe no op.
# - Never overwrite an existing log.expected to avoid masking changes.
# - Security note: Accepting log.today as baseline implies trust in the current
#   system state; review the first run output in sensitive contexts.
initialize_baseline() {
    # Idempotent guard: skip if baseline already exists
    if [ -f /var/log/chkrootkit/log.expected ]; then
        echo "[INFO] chkrootkit baseline already exists. Skipping initialization."
        return 0
    fi
    # Proceed only on Debian style wrapper that supports log.expected
    if [ -x /etc/cron.daily/chkrootkit ] && grep -q 'log\.expected' /etc/cron.daily/chkrootkit 2>/dev/null; then
        # Run once only when log.today is absent; suppress console noise
        if [ ! -f /var/log/chkrootkit/log.today ]; then
            echo "[INFO] Running daily chkrootkit wrapper to generate initial log.today..."
            sudo /etc/cron.daily/chkrootkit >/dev/null 2>&1 || true
        fi
        # Create baseline if today’s log is present
        if [ -f /var/log/chkrootkit/log.today ]; then
            echo "[INFO] Creating chkrootkit baseline at /var/log/chkrootkit/log.expected"
            sudo cp -a /var/log/chkrootkit/log.today /var/log/chkrootkit/log.expected
        else
            echo "[INFO] log.today not found. Baseline was not created."
        fi
    else
        echo "[INFO] No Debian style chkrootkit wrapper with log.expected support. Skipping baseline initialization."
    fi
}

# Perform installation steps
install() {
    # Perform initial checks
    check_system
    check_commands sudo cp chmod chown mkdir touch
    check_scripts
    check_sudo

    # Deploy chkrootkit cron job
    echo "[INFO] Deploying chkrootkit cron job."
    if ! sudo cp "$SCRIPTS/cron/bin/chkrootkit" /etc/cron.weekly/chkrootkit; then
        echo "[ERROR] Failed to copy chkrootkit cron script." >&2
        exit 1
    fi
    sudo chmod 740 /etc/cron.weekly/chkrootkit
    sudo chown root:adm /etc/cron.weekly/chkrootkit

    # Create log directory if it does not exist
    echo "[INFO] Ensuring /var/log/chkrootkit directory exists."
    if [ ! -d /var/log/chkrootkit ]; then
        if ! sudo mkdir -p /var/log/chkrootkit; then
            echo "[ERROR] Failed to create /var/log/chkrootkit directory." >&2
            exit 1
        fi
    fi
    sudo chmod 750 /var/log/chkrootkit
    sudo chown root:adm /var/log/chkrootkit

    # Set up chkrootkit log file
    echo "[INFO] Initializing chkrootkit log file."
    if [ ! -f /var/log/chkrootkit/chkrootkit.log ]; then
        sudo touch /var/log/chkrootkit/chkrootkit.log
    fi
    sudo chmod 640 /var/log/chkrootkit/chkrootkit.log
    sudo chown root:adm /var/log/chkrootkit/chkrootkit.log

    # Deploy log rotation configuration
    echo "[INFO] Deploying logrotate configuration for chkrootkit."
    if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/chkrootkit" /etc/logrotate.d/chkrootkit; then
        echo "[ERROR] Failed to deploy logrotate configuration." >&2
        exit 1
    fi
    sudo chmod 644 /etc/logrotate.d/chkrootkit
    sudo chown root:root /etc/logrotate.d/chkrootkit

    initialize_baseline

    echo "[INFO] chkrootkit setup completed successfully."
}

# Uninstall chkrootkit components
uninstall() {
    # Perform initial checks
    check_commands sudo rm
    check_sudo

    echo "[INFO] Removing chkrootkit cron job and logrotate configuration."

    if [ -f /etc/cron.weekly/chkrootkit ]; then
        sudo rm -v /etc/cron.weekly/chkrootkit
    else
        echo "[INFO] /etc/cron.weekly/chkrootkit not found. Skipping."
    fi

    if [ -f /etc/logrotate.d/chkrootkit ]; then
        sudo rm -v /etc/logrotate.d/chkrootkit
    else
        echo "[INFO] /etc/logrotate.d/chkrootkit not found. Skipping."
    fi

    echo "[INFO] Uninstallation completed. Log files under /var/log/chkrootkit are preserved."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        -u|--uninstall)
            uninstall
            ;;
        ""|*)
            install
            ;;
    esac

    return 0
}

# Execute main function
main "$@"
exit $?
