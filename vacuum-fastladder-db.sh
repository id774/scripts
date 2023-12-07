#!/bin/sh

########################################################################
# vacuum-fastladder-db.sh: Optimize Fastladder SQLite Database
#
# Description:
# This script is used to vacuum and optimize the Fastladder SQLite database.
# It creates a new database file from the existing database after running the vacuum command.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
#  v1.1 2023-12-07
#       Added check for sqlite3 program and updated documentation.
#  v1.0 2019-03-18
#       Initial release.
#
# Usage:
# Run the script from the command line:
#   ./vacuum-fastladder-db.sh
#
# Note:
# - Ensure that the Fastladder application is not actively using the database when this script is run.
# - This script should be run from the root directory of the Fastladder installation.
# - Backup the existing database before running this script as a safety precaution.
# - The script requires sqlite3 to be installed on the system.
#
########################################################################

# Check if sqlite3 is installed
if ! command -v sqlite3 >/dev/null 2>&1; then
    echo "Error: sqlite3 is not installed. Please install sqlite3 and try again."
    exit 1
fi

# Navigate to the Fastladder database directory
cd $HOME/fastladder/db
DB_PATH=fastladder/db/fastladder.db

# Remove existing temporary database file if it exists
test -f new.db && rm -vf new.db

# Vacuum the Fastladder SQLite database
sqlite3 $HOME/$DB_PATH vacuum

# Dump the current database and create a new optimized database
sqlite3 fastladder.db .dump | sqlite3 new.db

