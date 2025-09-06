#!/bin/sh

########################################################################
# debian_init.sh: Initial Setup Script for Ubuntu/Debian
#
#  Description:
#  Orchestrate the initial setup on Debian-based systems by running the
#  following phases in order:
#    - Environment bootstrap (debian_env.sh):
#        * Locale setup (ja_JP.UTF-8), system update/upgrade/cleanup,
#          admin/wheel groups, ext FS tuning via setup_tune2fs.sh
#    - Bulk package install (debian_apt.sh):
#        * Install categorized package groups (base tools, toolchains, system &
#          networking utils, Debian packaging toolchain, editors/TeX, EXIF tools,
#          languages & libs, SCM, DBs, SQLite, optional dev headers)
#    - System customization (debian_setup.sh):
#        * Set default shells (zsh), deploy dotfiles (zsh/vim/emacs),
#          install crypto tools (TrueCrypt/VeraCrypt), sysadmin helpers,
#          monitoring & services (munin, memcached), security configs
#          (iptables, PAM, securetty, rsyslog cron, chkrootkit opts),
#          user env (crontab, aliases, MOTD), IPython dotfiles,
#          permissions fixes and sysctl apply, cleanup of shell history
#    - (Optional) Desktop provisioning:
#        * Desktop packages (debian_desktop_apt.sh) and DE specific settings
#          (debian_desktop_setup.sh) can be included by passing a desktop option.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_init.sh                    # base setup only
#      ./debian_init.sh --xfce             # include XFCE desktop provisioning
#      ./debian_init.sh --gnome-flashback  # include GNOME Flashback provisioning
#
#  Description:
#  - Run the script directly; it will configure base system setup.
#  - Use --xfce or --gnome-flashback to include desktop provisioning steps.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for package installations.
#  - Review and modify the installation scripts as needed before execution.
#  - By default, only minimal system setup is applied. Desktop provisioning
#    runs only when --xfce or --gnome-flashback is specified to avoid
#    accidental GUI stack installation on server hosts.
#
#  Error Conditions:
#  - If the system is not Debian-based, the script exits with an error.
#  - If the required directory does not exist, an error message is displayed.
#  - Errors from underlying scripts should be resolved based on their output.
#
#  Version History:
#  v6.1 2025-09-06
#       Replace --with-desktop with --xfce or --gnome-flashback and pass through
#       the selected option to debian_desktop_setup.sh.
#  v6.0 2025-08-20
#       Add --with-desktop option to trigger desktop setup steps.
#       Expanded header documentation to enumerate orchestrated phases and clarify
#       optional desktop provisioning entry points for consistency and transparency.
#  v5.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v5.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v5.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v5.0 2025-03-21
#       Improved system detection for Debian and Ubuntu.
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#       Redirected error messages to stderr for better logging and debugging.
#       Added confirmation prompt before execution.
#  [Further version history truncated for brevity]
#  v0.1 2007-08-27
#       First version.
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

# Verify if the system is Debian-based
check_debian_based() {
    if [ ! -f /etc/os-release ]; then
        echo "[ERROR] Unable to determine the operating system." >&2
        exit 1
    fi

    # Extracting OS information
    OS_ID=$(awk -F= '/^ID=/{print $2}' /etc/os-release | tr -d '"')
    OS_LIKE=$(awk -F= '/^ID_LIKE=/{print $2}' /etc/os-release | tr -d '"')

    # Checking if the system is Debian-based
    case "$OS_ID" in
        debian|ubuntu) return 0 ;;
        *)
            case "$OS_LIKE" in
                *debian*) return 0 ;;
                *)
                    echo "[ERROR] This script is intended for Debian or Ubuntu systems only." >&2
                    exit 1
                    ;;
            esac
            ;;
    esac
}

# Verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "[ERROR] Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Ask for confirmation before execution
confirm_execution() {
    echo "[INFO] This script will configure your Debian-based system."
    printf "[INFO] Do you want to proceed? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y|yes|YES) return 0 ;;
        *) echo "[ERROR] Aborted by user."; exit 1 ;;
    esac
}

# Main entry point of the script
main() {
    # Handle help and desktop options before any environment checks
    DESKTOP_OPTION=""
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        --xfce|--gnome-flashback)
            DESKTOP_OPTION="$1"
            if [ $# -gt 1 ]; then
                echo "[ERROR] Too many arguments. Only one option is allowed." >&2
                usage
            fi
            ;;
        "" )
            : # no option
            ;;
        * )
            echo "[ERROR] Unknown option: $1" >&2
            usage
            ;;
    esac

    # Check if the system is Debian-based before proceeding
    check_system
    setup_environment
    check_commands awk tr
    check_debian_based

    # Ask for confirmation before proceeding
    confirm_execution

    echo "[INFO] Running base system setup"

    # Environment setup
    "$SCRIPTS/installer/debian_env.sh"

    # Package installation
    "$SCRIPTS/installer/debian_apt.sh"

    # System customization
    "$SCRIPTS/installer/debian_setup.sh"

    # Optional desktop provisioning when a desktop option is provided
    if [ -n "$DESKTOP_OPTION" ]; then
        echo "[INFO] Running desktop setup as requested: $DESKTOP_OPTION"
        "$SCRIPTS/installer/debian_desktop_apt.sh"
        "$SCRIPTS/installer/debian_desktop_setup.sh" "$DESKTOP_OPTION"
    fi

    echo "[INFO] All Debian initial setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?
