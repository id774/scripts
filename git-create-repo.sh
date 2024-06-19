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
#  v1.3 2024-06-19
#       Added --use-sudo and --no-sudo options to explicitly control sudo usage.
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
#  To create a new repository:
#      git-create-repo.sh <repository_name> [repository_path] [--dry-run] [--use-sudo] [--no-sudo]
#
#  To delete an existing repository:
#      git-create-repo.sh <repository_name> [repository_path] [--delete] [--use-sudo] [--no-sudo]
#
#  For help:
#      git-create-repo.sh -h
#
#  Note: The script may require sudo permissions for certain operations,
#  especially when dealing with system-wide paths like /var/lib/git. By default,
#  sudo is used for system-wide paths and not used for paths within the user's
#  home directory. This behavior can be overridden with the --use-sudo or --no-sudo options.
#
########################################################################

# Display script usage information
usage() {
    cat << EOF
Usage: $0 <repository_name> [repository_path] [--dry-run] [--delete] [--use-sudo] [--no-sudo]

This script automates the creation and deletion of Git repositories.

Options:
  --dry-run           Show what would be done without making any changes.
  --delete            Delete the specified repository instead of creating it.
  --use-sudo          Explicitly use sudo for all operations, regardless of the repository path.
  --no-sudo           Explicitly do not use sudo for any operations, regardless of the repository path.

Default Behavior:
- The default repository path is /var/lib/git if not specified.
- Sudo is used by default for system-wide paths like /var/lib/git.
- Sudo is not used by default for paths within the user's home directory.

Examples:
  Create a new repository at /var/lib/git:
    $0 myrepo

  Create a new repository at a custom path without sudo:
    $0 myrepo /home/user/myrepo --no-sudo

  Delete an existing repository at /var/lib/git:
    $0 myrepo --delete

  Show what would be done without making changes:
    $0 myrepo /custom/path --dry-run
EOF
    exit 1
}

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Git is not installed. This script requires Git for managing Git repositories. Please install Git and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check if a directory exists
check_directory() {
    local dir=$1
    if [ ! -d "$dir" ]; then
        echo "Error: Directory '$dir' does not exist."
        exit 3
    fi
}

# Check if a directory is a Git repository
is_git_repository() {
    local repo_path=$1
    if [ -d "${repo_path}/.git" ] || ( [ -f "${repo_path}/HEAD" ] && [ -d "${repo_path}/objects" ] ); then
        return 0
    else
        return 1
    fi
}

# Create a new Git repository with proper permissions
create_git_repo() {
    local repo_name=$1
    local repo_path=$2
    local dry_run=$3
    local use_sudo=$4

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
        $use_sudo chown -R git:git "${repo_path}"
    fi

    echo "Repository '${repo_name}' created at '${repo_path}'"
}

# Delete a Git repository
delete_git_repo() {
    local repo_path=$1
    local use_sudo=$2

    if is_git_repository "$repo_path"; then
        $use_sudo rm -rf "${repo_path}"
        echo "Repository at '${repo_path}' has been deleted."
    else
        echo "Error: '${repo_path}' is not a Git repository."
        exit 4
    fi
}

# Main script execution starts here

dry_run=false
delete_repo=false
explicit_sudo=""

# Parse options
while [ $# -gt 0 ]; do
    case "$1" in
        --dry-run) dry_run=true ;;
        --delete) delete_repo=true ;;
        --use-sudo) explicit_sudo="sudo" ;;
        --no-sudo) explicit_sudo="" ;;
        -h|--help) usage ;;
        -*|--*) usage ;;
        *) break ;;
    esac
    shift
done

# Check the number of arguments
if [ "$#" -lt 1 ]; then
    usage
fi

# Check if Git is installed
check_commands git

repo_name=$1
repo_base_path=${2:-"/var/lib/git"}
repo_full_path="${repo_base_path}/${repo_name}.git"

# Determine sudo usage if not explicitly set
if [ -z "$explicit_sudo" ]; then
    if [[ "${repo_base_path}" == "${HOME}"* ]]; then
        use_sudo=""
    else
        use_sudo="sudo"
    fi
else
    use_sudo="$explicit_sudo"
fi

if [ "$delete_repo" = true ]; then
    delete_git_repo "${repo_full_path}" "$use_sudo"
    exit 0
fi

check_directory "$repo_base_path"
create_git_repo "${repo_name}" "${repo_full_path}" "$dry_run" "$use_sudo"
