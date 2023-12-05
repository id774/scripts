#!/bin/sh
#
########################################################################
# Git Repository Management Script
#
#  Description:
#  This script automates the creation and deletion of Git repositories.
#  It allows for setting a custom path for the repository and includes options
#  for a dry run and for deleting an existing repository. The default repository
#  path is set to /var/lib/git if not specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-11-26
#       Initial release. Features include creating and deleting Git repositories,
#       dry run option for creation, and custom repository path setting.
#
# Usage:
#  To create a new repository:
#      git-create-repo.sh <repository_name> [repository_path] [--dry-run]
#
#  To delete an existing repository:
#      git-create-repo.sh <repository_name> [repository_path] [--delete]
#
#  For help:
#      git-create-repo.sh -h
#
#  Note: The script may require sudo permissions for certain operations,
#  especially when dealing with system-wide paths like /var/lib/git.
#
########################################################################

# Display script usage information
usage() {
    echo "Usage: $0 <repository_name> [repository_path] [--dry-run] [--delete]"
    echo "Default repository path is /var/lib/git if not specified."
    exit 1
}

# Check if a directory exists
check_directory() {
    local dir=$1
    if [ ! -d "$dir" ]; then
        echo "Error: Directory '$dir' does not exist."
        exit 2
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

    # Check if the repo path is under the home directory
    local use_sudo="sudo"
    if [[ "${repo_path}" == "${HOME}"* ]]; then
        use_sudo=""
    fi

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

    # Check if the repo path is under the home directory
    local use_sudo="sudo"
    if [[ "${repo_path}" == "${HOME}"* ]]; then
        use_sudo=""
    fi

    if is_git_repository "$repo_path"; then
        $use_sudo rm -rf "${repo_path}"
        echo "Repository at '${repo_path}' has been deleted."
    else
        echo "Error: '${repo_path}' is not a Git repository."
        exit 3
    fi
}

# Main script execution starts here

dry_run=false
delete_repo=false

# Parse options
while [ $# -gt 0 ]; do
    case "$1" in
        --dry-run) dry_run=true ;;
        --delete) delete_repo=true ;;
        -*|--*) usage ;;
        *) break ;;
    esac
    shift
done

# Check the number of arguments
if [ "$#" -lt 1 ]; then
    usage
fi

repo_name=$1
repo_base_path=${2:-"/var/lib/git"}
repo_full_path="${repo_base_path}/${repo_name}.git"

if [ "$delete_repo" = true ]; then
    delete_git_repo "${repo_full_path}"
    exit 0
fi

check_directory "$repo_base_path"
create_git_repo "${repo_name}" "${repo_full_path}" "$dry_run"

