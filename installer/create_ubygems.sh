#!/bin/sh

########################################################################
# create_ubygems.sh: Script to Create ubygems.rb in Ruby's rubylibdir
#
#  Description:
#  This script retrieves Ruby's standard library directory (`rubylibdir`)
#  using the Homebrew-installed Ruby and creates a `ubygems.rb` file in
#  that directory. The file contains a simple `require 'rubygems'` statement
#  to restore compatibility with older Ruby versions that expected this file.
#  The primary purpose of this script is to maintain compatibility for tools
#  and plugins, such as older Vim plugins, that rely on `ubygems.rb` being present.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-27
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./create_ubygems.sh
#
#  Requirements:
#  - Homebrew must be installed, and Ruby must be installed via Homebrew.
#  - The user must have write permissions for the Ruby standard library directory.
#
#  Notes:
#  - Ensure that creating `ubygems.rb` does not conflict with existing configurations.
#  - This script assumes `rubylibdir` is writable. If not, use `sudo` or check permissions.
#  - Be aware that this change is primarily for compatibility purposes and may not be
#    necessary for modern tools or workflows.
#  - Verify the file creation by listing the directory contents after execution.
#  - If `ubygems.rb` already exists, the script will terminate without overwriting it.
#
########################################################################

# Define the Ruby binary path
if [ -x "/opt/homebrew/opt/ruby/bin/ruby" ]; then
    RUBY_BIN="/opt/homebrew/opt/ruby/bin/ruby"
elif [ -x "/usr/local/opt/ruby/bin/ruby" ]; then
    RUBY_BIN="/usr/local/opt/ruby/bin/ruby"
else
    echo "Error: Ruby is not installed via Homebrew or the expected path is incorrect." >&2
    exit 1
fi

# Retrieve the rubylibdir with RUBYOPT disabled
TARGET_DIR=$(RUBYOPT="" $RUBY_BIN -e 'puts RbConfig::CONFIG["rubylibdir"]')

# Check if the directory retrieval was successful
if [ -z "$TARGET_DIR" ]; then
    echo "Error: Unable to retrieve the Ruby library directory." >&2
    exit 1
fi

# Check if ubygems.rb already exists or if it is required
if [ -f "$TARGET_DIR/ubygems.rb" ]; then
    echo "Notice: ubygems.rb already exists in $TARGET_DIR. No changes were made."
    ls -l "$TARGET_DIR/ubygems.rb"
    cat "$TARGET_DIR/ubygems.rb"
    exit 0
else
    echo "Creating ubygems.rb in $TARGET_DIR as it does not exist."
fi

# Create ubygems.rb
RUBYOPT="" $RUBY_BIN -e "File.write(File.join('$TARGET_DIR', 'ubygems.rb'), \"require 'rubygems'\\n\")"

# Check if the file was successfully created
if [ $? -eq 0 ]; then
    echo "ubygems.rb was successfully created in $TARGET_DIR"
    echo "Listing the contents of $TARGET_DIR:"
    ls -l "$TARGET_DIR/ubygems.rb"
    echo "Contents of ubygems.rb:"
    cat "$TARGET_DIR/ubygems.rb"
else
    echo "Error: Failed to create ubygems.rb." >&2
    exit 1
fi
