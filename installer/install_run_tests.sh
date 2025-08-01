#!/bin/sh

########################################################################
# install_run_tests.sh: Deploy run_tests and Configure Cron Job
#
#  Description:
#  This script automates the deployment of the run_tests.sh script and its
#  configuration file for automated testing of Python and Ruby projects.
#  It ensures the necessary directories exist, copies the script and its
#  configuration file to secure locations, sets appropriate permissions,
#  and schedules the tests to run automatically via a cron job.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Execute this script with sufficient permissions to perform directory
#  creation, file copying, and cron job scheduling tasks:
#      ./install_run_tests.sh
#
#  Notes:
#  - The SCRIPTS environment variable must be set to the directory containing
#    the run_tests script and its configuration file before running this script.
#  - After deployment, review and potentially edit /etc/cron.config/run_tests.conf
#    and /etc/cron.d/run_tests to finalize the configuration.
#
#  Version History:
#  v2.2 2025-07-30
#       Move script to /etc/cron.exec and config to /etc/cron.config.
#  v2.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.0 2025-04-29
#       Add error handling for critical copy and cron setup steps.
#       Use sudo when checking existence of files and directories
#       to handle permission-restricted environments safely.
#  v1.9 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.8 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.7 2025-04-11
#       Prevent overwriting existing configuration file during deployment.
#  v1.6 2025-03-27
#       Refactored final messages and updated cron job time.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-21
#       Refactored script by functionizing all processes without changing behavior.
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.1 2024-03-17
#       Added log file creation in /var/log/sysadmin and deployed log rotation configuration.
#  v1.0 2024-03-13
#       Initial deployment script for automated testing setup.
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
    if [ "$(uname -s)" != "Linux" ]; then
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set it to the directory containing the run_tests script." >&2
        exit 1
    fi
}

# Create directory for logs
setup_log_directory() {
    if ! sudo test -d /var/log/sysadmin; then
        echo "[INFO] Creating /var/log/sysadmin."
        if ! sudo mkdir -p /var/log/sysadmin; then
            echo "[ERROR] Failed to create /var/log/sysadmin" >&2
            exit 1
        fi
    fi

    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin
}

# Set up log file and permissions
setup_log_file() {
    if ! sudo test -f /var/log/sysadmin/run_tests.log; then
        echo "[INFO] Creating run_tests log file."
        if ! sudo touch /var/log/sysadmin/run_tests.log; then
            echo "[ERROR] Failed to create /var/log/sysadmin/run_tests.log" >&2
            exit 1
        fi
    fi

    sudo chmod 640 /var/log/sysadmin/run_tests.log
    sudo chown root:adm /var/log/sysadmin/run_tests.log
}

# Deploy log rotation configuration
deploy_log_rotation() {
    if ! sudo test -f /etc/logrotate.d/run_tests; then
        echo "[INFO] Deploying logrotate configuration."
        if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/run_tests" /etc/logrotate.d/run_tests; then
            echo "[ERROR] Failed to copy logrotate configuration" >&2
            exit 1
        fi
    fi

    sudo chmod 640 /etc/logrotate.d/run_tests
    sudo chown root:adm /etc/logrotate.d/run_tests
}

# Deploy run_tests script and configuration file
deploy_scripts() {
    echo "[INFO] Copying run_tests to /etc/cron.exec."
    if ! sudo mkdir -p /etc/cron.exec; then
        echo "[ERROR] Failed to create /etc/cron.exec." >&2
        exit 1
    fi

    sudo chmod 750 /etc/cron.exec
    sudo chown root:adm /etc/cron.exec

    if ! sudo cp "$SCRIPTS/cron/bin/run_tests" /etc/cron.exec/; then
        echo "[ERROR] Failed to copy run_tests to /etc/cron.exec." >&2
        exit 1
    fi

    CONFIG_DIR="/etc/cron.config"
    CONFIG_FILE="/etc/cron.config/run_tests.conf"

    if ! sudo mkdir -p "$CONFIG_DIR"; then
        echo "[ERROR] Failed to create $CONFIG_DIR." >&2
        exit 1
    fi
    sudo chmod 750 "$CONFIG_DIR"
    sudo chown root:adm "$CONFIG_DIR"

    if ! sudo test -f "$CONFIG_FILE"; then
        echo "[INFO] Copying run_tests.conf to /etc/cron.config."
        if ! sudo cp "$SCRIPTS/cron/etc/run_tests.conf" "$CONFIG_FILE"; then
            echo "[ERROR] Failed to copy run_tests.conf" >&2
            exit 1
        fi
    else
        echo "[INFO] Configuration already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi

    sudo chmod 740 /etc/cron.exec/run_tests
    sudo chown root:adm /etc/cron.exec/run_tests
    sudo chmod 640 "$CONFIG_FILE"
    sudo chown root:adm "$CONFIG_FILE"
}

# Set up cron job for running tests
setup_cron_job() {
    CRON_FILE="/etc/cron.d/run_tests"
    CRON_JOB="30 04 * * * root test -x /etc/cron.exec/run_tests && /etc/cron.exec/run_tests"

    if sudo test -f "$CRON_FILE"; then
        echo "[INFO] Cron job already exists: $CRON_FILE"
        echo "[INFO] Skipping creation to preserve existing configuration."
    else
        echo "[INFO] Creating cron job at /etc/cron.d/run_tests."
        if ! echo "$CRON_JOB" | sudo tee "$CRON_FILE" > /dev/null; then
            echo "[ERROR] Failed to create cron job file" >&2
            exit 1
        fi
    fi

    sudo chmod 640 "$CRON_FILE"
    sudo chown root:adm "$CRON_FILE"
}

# Print post-installation instructions and next steps
final_message() {
    echo "[INFO] Installation of run_tests setup completed successfully."
    echo "# Notes: Please review '/etc/cron.config/run_tests.conf' and '/etc/cron.d/run_tests'"
    echo "# to ensure all settings meet your operational requirements."
}

# Main function to execute all setup tasks
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_commands sudo cp chmod chown touch mkdir tee
    check_sudo
    setup_log_directory
    setup_log_file
    deploy_log_rotation
    deploy_scripts
    setup_cron_job
    final_message
    return 0
}

# Execute main function
main "$@"
exit $?
