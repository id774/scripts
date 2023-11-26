#!/bin/sh

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

    if [ "$dry_run" = true ]; then
        echo "Dry run: A new repository would be created at '${repo_path}'"
        return 0
    fi

    sudo mkdir -p "${repo_path}" || return 1
    cd "${repo_path}" || return 1
    sudo git init --bare --shared || return 1
    sudo chmod -R o-rwx,g+ws "${repo_path}" || return 1
    sudo chown -R git:git "${repo_path}" || return 1
    echo "Repository '${repo_name}' created at '${repo_path}'"
}

# Delete a Git repository
delete_git_repo() {
    local repo_path=$1

    if is_git_repository "$repo_path"; then
        sudo rm -rf "${repo_path}"
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

