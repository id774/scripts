#!/bin/bash

########################################################################
# run_tests.sh: Run all Python and Ruby tests in the test directory
#
# Description:
# This script checks for the presence of Python and Ruby, and executes all 
# Python and Ruby test files located in the 'test' subdirectory. It displays
# the paths and versions of Python and Ruby being used.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
# v1.0 2023-12-15
#       First release of the test script.
#
# Usage:
# Run this script from the command line to execute all tests:
# ./run_tests.sh
#
########################################################################

# Check if SCRIPTS variable is set
if [ -z "$SCRIPTS" ]; then
    echo "Error: SCRIPTS environment variable is not set."
    echo "Please set the SCRIPTS variable to the path of your script collection."
    exit 1
fi

cd $SCRIPTS

# Check if Python is installed
if ! command -v python &> /dev/null; then
    echo "Python is not installed. Skipping Python tests."
else
    # Display Python path and version
    echo "Python path: $(which python)"
    python --version

    # Execute Python tests
    for file in test/*_test.py; do
        echo "Running Python test: $file"
        python "$file"
    done
    echo "All Python tests completed."
fi

# Check if Ruby is installed
if ! command -v ruby &> /dev/null; then
    echo "Ruby is not installed. Skipping Ruby tests."
else
    # Display Ruby path and version
    echo "Ruby path: $(which ruby)"
    ruby --version

    # Check if 'rspec' is installed for Ruby tests
    if ! command -v rspec &> /dev/null; then
        echo "'rspec' is not installed. Skipping Ruby tests."
    else
        # Execute Ruby tests
        for file in test/*_test.rb; do
            echo "Running Ruby test: $file"
            rspec "$file"
        done
        echo "All Ruby tests completed."
    fi
fi

echo "All tests completed."
