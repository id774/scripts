#!/bin/sh

########################################################################
# create_ubygems.sh: Script to Create ubygems.rb in Ruby's rubylibdir
#
#  Description:
#  This script retrieves Ruby's standard library directory (`rubylibdir`)
#  using either the Homebrew-installed Ruby (default behavior) or a
#  user-specified Ruby installation path. It then creates a `ubygems.rb`
#  file in the directory, containing a simple `require 'rubygems'` statement.
#
#  The primary purpose of this script is to maintain compatibility for
#  tools and plugins, such as older Vim plugins, that rely on `ubygems.rb`
#  being present.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-19
#       Added support for custom Ruby installation paths.
#       Improved error handling for missing Ruby directories.
#       Enhanced script documentation and comments.
#  v1.0 2025-01-27
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments (default Homebrew behavior):
#      ./create_ubygems.sh
#
#  Specify a custom Ruby installation path:
#      ./create_ubygems.sh /opt/ruby/3.4
#
#  Requirements:
#  - Ruby must be installed and accessible in the system.
#  - The user must have write permissions for the Ruby standard library directory.
#
#  Notes:
#  - Ensure that creating `ubygems.rb` does not conflict with existing configurations.
#  - If `ubygems.rb` already exists, the script will terminate without overwriting it.
#  - When a custom Ruby installation path is specified, the script searches
#    for a `x.y.z` version directory under `lib/ruby/`.
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

# Function to check required commands before execution
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

# Function to determine the Ruby library path for a custom installation
get_custom_ruby_path() {
    BASE_DIR="$1"

    # Ensure the given path contains a valid Ruby library directory
    if [ ! -d "$BASE_DIR/lib/ruby" ]; then
        echo "[ERROR] Directory $BASE_DIR/lib/ruby does not exist." >&2
        exit 1
    fi

    # Find the latest x.y.z format directory under lib/ruby
    RUBY_VERSION=$(find "$BASE_DIR/lib/ruby" -maxdepth 1 -type d | grep -E '/[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n1)

    # Ensure a valid directory was found
    if [ -z "$RUBY_VERSION" ]; then
        echo "[ERROR] No valid Ruby version directory found under $BASE_DIR/lib/ruby/." >&2
        exit 1
    fi

    # Extract only the x.y.z part and set target directory
    RUBY_VERSION=$(basename "$RUBY_VERSION")
    TARGET_DIR="$BASE_DIR/lib/ruby/$RUBY_VERSION"
}

# Function to determine the Ruby library path for Homebrew-installed Ruby
get_homebrew_ruby_path() {
    if [ -x "/opt/homebrew/opt/ruby/bin/ruby" ]; then
        RUBY_BIN="/opt/homebrew/opt/ruby/bin/ruby"
    elif [ -x "/usr/local/opt/ruby/bin/ruby" ]; then
        RUBY_BIN="/usr/local/opt/ruby/bin/ruby"
    else
        echo "[ERROR] Ruby is not installed via Homebrew or the expected path is incorrect." >&2
        exit 1
    fi

    # Retrieve the Ruby library directory using Homebrew Ruby
    TARGET_DIR=$(RUBYOPT="" $RUBY_BIN -e 'puts RbConfig::CONFIG["rubylibdir"]')

    # Check if the directory retrieval was successful
    if [ -z "$TARGET_DIR" ]; then
        echo "[ERROR] Unable to retrieve the Ruby library directory." >&2
        exit 1
    fi
}

# Function to create ubygems.rb if it does not already exist
create_ubygems() {
    if [ -f "$TARGET_DIR/ubygems.rb" ]; then
        echo "Notice: ubygems.rb already exists in $TARGET_DIR. No changes were made."
        ls -l "$TARGET_DIR/ubygems.rb"
        cat "$TARGET_DIR/ubygems.rb"
        exit 0
    else
        echo "Creating ubygems.rb in $TARGET_DIR as it does not exist."
    fi

    # Create ubygems.rb with a simple 'require rubygems' statement
    printf "require 'rubygems'\\n" > "$TARGET_DIR/ubygems.rb"

    # Verify that the file was successfully created
    if [ $? -eq 0 ]; then
        echo "ubygems.rb was successfully created in $TARGET_DIR"
        echo "Listing the contents of $TARGET_DIR:"
        ls -l "$TARGET_DIR/ubygems.rb"
        echo "Contents of ubygems.rb:"
        cat "$TARGET_DIR/ubygems.rb"
    else
        echo "[ERROR] Failed to create ubygems.rb." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Ensure required commands are available before proceeding
    check_commands find grep sort tail basename ls cat

    # Determine target directory
    if [ -n "$1" ]; then
        get_custom_ruby_path "$1"
    else
        get_homebrew_ruby_path
    fi

    # Ensure the target directory exists
    if [ ! -d "$TARGET_DIR" ]; then
        echo "[ERROR] Target directory $TARGET_DIR does not exist." >&2
        exit 1
    fi

    # Create ubygems.rb if necessary
    create_ubygems
}

# Execute main function
main "$@"
