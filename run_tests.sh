#!/bin/sh

########################################################################
# run_tests.sh: Run all Python and Ruby tests in the test directory
#
#  Description:
#  This script checks for the presence of Python and Ruby, and executes all 
#  Python and Ruby test files located in the 'test' subdirectory. It displays
#  the paths and versions of Python and Ruby being used, and checks if each
#  test passes or fails.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2024-03-06
#       Added checks to ensure specified Python and RSpec paths are not only
#       non-empty but also point to executable files. This enhancement
#       prevents the execution of tests with invalid paths.
#  v1.3 2024-01-14
#       Added the ability to specify custom Python and RSpec paths
#       as command-line arguments.
#  v1.2 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.1 2023-12-20
#       Added environment variable to prevent the creation of __pycache__
#       directories during Python tests.
#       Replaced 'which' with 'command -v' for command existence check.
#  v1.0 2023-12-15
#       First release of the test script.
#
#  Usage:
#  Run this script from the command line to execute all tests:
#  ./run_tests.sh [Python path] [RSpec path]
#  If no paths are specified, it will use the default system paths.
#
########################################################################

export PYTHONDONTWRITEBYTECODE=1

# Check if SCRIPTS variable is set
if [ -z "$SCRIPTS" ]; then
    echo "Error: SCRIPTS environment variable is not set."
    echo "Please set the SCRIPTS variable to the path of your script collection."
    exit 1
fi

cd "$SCRIPTS" || exit

python_path="$1"
rspec_path="$2"

# Initialize failure counters
python_failures=0
ruby_failures=0

# Check if Python is installed
if [ -z "$python_path" ]; then
    python_path=$(command -v python)
fi

if [ -z "$python_path" ]; then
    echo "Python is not installed. Skipping Python tests."
else
    if [ ! -x "$python_path" ]; then
        echo "Error: Specified Python path is either invalid or not executable."
        exit 1
    fi
    # Display Python path and version
    echo "Python path: $python_path"
    "$python_path" --version

    # Execute Python tests
    for file in test/*_test.py; do
        echo "Running Python test: $file"
        output="$("$python_path" "$file" 2>&1)" # Capture both stdout and stderr
        echo "$output"
        if ! echo "$output" | grep -qE "OK|SKIPPED|OK \(skipped=[0-9]+\)" ; then
            echo "Failure in Python test: $file"
            python_failures=$(expr $python_failures + 1)
        fi
    done
    echo "All Python tests completed."
fi

# Check if RSpec is installed
if [ -z "$rspec_path" ]; then
    rspec_path=$(command -v rspec)
fi

if [ -z "$rspec_path" ]; then
    echo "RSpec is not installed. Skipping Ruby tests."
else
    if [ ! -x "$rspec_path" ]; then
        echo "Error: Specified RSpec path is either invalid or not executable."
        exit 1
    fi
    # Display RSpec path and version
    echo "RSpec path: $rspec_path"
    ruby_dir="$(dirname "$rspec_path")"
    ruby_command="$ruby_dir/ruby"
    "$ruby_command" --version
    "$rspec_path" --version

    # Execute Ruby tests
    for file in test/*_test.rb; do
        echo "Running Ruby test: $file"
        output="$("$rspec_path" "$file" 2>&1)" # Capture both stdout and stderr
        echo "$output"
        if ! echo "$output" | grep -q "0 failures"; then
            echo "Failure in Ruby test: $file"
            ruby_failures=$(expr $ruby_failures + 1)
        fi
    done
    echo "All Ruby tests completed."
fi

# Final report
if [ "$python_failures" -ne 0 ] || [ "$ruby_failures" -ne 0 ]; then
    echo "Some tests failed. Python failures: $python_failures, Ruby failures: $ruby_failures."
    exit 1
else
    echo "All tests passed successfully."
    exit 0
fi

