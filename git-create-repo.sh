#!/bin/sh

########################################################################
# git-create-repo.sh: Git Repository Management Script
#
#  Description:
#  This script automates the creation and deletion of Git repositories.
#  It allows for setting a custom path for the repository and includes options
#  for a dry run, deleting an existing repository, and explicitly controlling
#  the use of sudo. The default repository path is set to /var/lib/git if not
#  specified. It checks for Git installation before proceeding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.6 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.5 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.4 2024-06-23
#       Changed the exit code from 1 to 0 for help message display.
#  v1.3 2024-06-19
#       Added --sudo and --no-sudo options to explicitly control sudo usage.
#       Fixed bug where --dry-run option did not work during repository deletion.
#       Added options to specify user and group for chown.
#  v1.2 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2023-11-26
#       Initial release. Features include creating and deleting Git repositories,
#       dry run option for creation, and custom repository path setting.
#
#  Usage:
#  This script automates the creation and deletion of Git repositories.
#
#  To create a new repository:
#      ./git-create-repo.sh <repository_name> [repository_path] [--dry-run] [--sudo] [--no-sudo] [--user USER] [--group GROUP]
#
#  To delete an existing repository:
#      ./git-create-repo.sh <repository_name> [repository_path] [--delete] [--dry-run] [--sudo] [--no-sudo] [--user USER] [--group GROUP]
#
#  For help:
#      ./git-create-repo.sh -h
#
#  Options:
#  --dry-run           Show what would be done without making any changes.
#  --delete            Delete the specified repository instead of creating it.
#  --sudo              Explicitly use sudo for all operations, regardless of the repository path.
#  --no-sudo           Explicitly do not use sudo for any operations, regardless of the repository path.
#  --user USER         Specify the user for chown (default: git).
#  --group GROUP       Specify the group for chown (default: git).
#
#  Examples:
#    Create a new repository at /var/lib/git:
#      git-create-repo.sh myrepo
#
#    Create a new repository at a custom path without sudo:
#      git-create-repo.sh myrepo /home/user/myrepo --no-sudo
#
#    Delete an existing repository at /var/lib/git:
#      git-create-repo.sh myrepo --delete
#
#    Show what would be done without making changes:
#      git-create-repo.sh myrepo /custom/path --dry-run
#
#  Default Behavior:
#  - The default repository path is /var/lib/git if not specified.
#  - Sudo is used by default for system-wide paths like /var/lib/git.
#  - Sudo is not used by default for paths within the user's home directory.
#
#  Notes: The script may require sudo permissions for certain operations,
#  especially when dealing with system-wide paths like /var/lib/git. By default,
#  sudo is used for system-wide paths and not used for paths within the user's
#  home directory. This behavior can be overridden with the --sudo or --no-sudo options.
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if necessary commands exist
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if a directory exists
check_directory() {
    dir=$1
    if [ ! -d "$dir" ]; then
        echo "Error: Directory '$dir' does not exist." >&2
        exit 3
    fi
}

# Check if a directory is a Git repository
is_git_repository() {
    repo_path=$1
    use_sudo=$2
    if $use_sudo [ -d "${repo_path}/.git" ] || { $use_sudo [ -f "${repo_path}/HEAD" ] && $use_sudo [ -d "${repo_path}/objects" ]; }; then
        return 0
    else
        return 1
    fi
}

# Create a new Git repository with proper permissions
create_git_repo() {
    repo_name=$1
    repo_path=$2
    dry_run=$3
    use_sudo=$4
    user=$5
    group=$6

    if [ "$dry_run" = true ]; then
        echo "Dry run: A new repository would be created at '${repo_path}'"
        return 0
    fi

    $use_sudo mkdir -p "${repo_path}" || return 1
    cd "${repo_path}" || return 1
    $use_sudo git init --bare --shared || return 1
    $use_sudo chmod -R o-rwx,g+ws "${repo_path}" || return 1

    if [ -z "$use_sudo" ]; then
        chmod -R u+rwX "${repo_path}"
    else
        $use_sudo chown -R "${user}:${group}" "${repo_path}"
    fi

    echo "Repository '${repo_name}' created at '${repo_path}'"
}

# Delete a Git repository
delete_git_repo() {
    repo_path=$1
    dry_run=$2
    use_sudo=$3

    if [ "$dry_run" = true ]; then
        echo "Dry run: The repository at '${repo_path}' would be deleted."
        return 0
    fi

    if is_git_repository "$repo_path" "$use_sudo"; then
        $use_sudo rm -rf "${repo_path}"
        echo "Repository at '${repo_path}' has been deleted."
    else
        echo "Error: '${repo_path}' is not a Git repository." >&2
        exit 4
    fi
}

# Parse command-line arguments
parse_arguments() {
    dry_run=false
    delete_repo=false
    explicit_sudo=""
    user="git"
    group="git"

    while [ $# -gt 0 ]; do
        case "$1" in
            --dry-run) dry_run=true ;;
            --delete) delete_repo=true ;;
            --sudo) explicit_sudo="sudo" ;;
            --no-sudo) explicit_sudo="" ;;
            --user) user="$2"; shift ;;
            --group) group="$2"; shift ;;
            -h|--help) usage ;;
            --) shift; break ;;
            -*) usage ;;
            *) break ;;
        esac
        shift
    done

    if [ "$#" -lt 1 ]; then
        usage
    fi

    repo_name=$1
    repo_base_path=${2:-"/var/lib/git"}
    repo_full_path="${repo_base_path}/${repo_name}.git"

    if [ -z "$explicit_sudo" ]; then
        case "$repo_base_path" in
            $HOME*) use_sudo="" ;;
            *) use_sudo="sudo" ;;
        esac
    else
        use_sudo="$explicit_sudo"
    fi
}

# Main function to execute the script
main() {
    parse_arguments "$@"

    check_commands awk git

    if [ "$use_sudo" = "sudo" ]; then
        check_sudo
    fi

    if [ "$delete_repo" = true ]; then
        delete_git_repo "${repo_full_path}" "$dry_run" "$use_sudo"
        exit 0
    fi

    check_directory "$repo_base_path"
    create_git_repo "${repo_name}" "${repo_full_path}" "$dry_run" "$use_sudo" "$user" "$group"
}

# Execute main function
main "$@"
