#!/bin/bash

########################################################################
# run_tests.sh: Run all Python and Ruby tests in the test directory
#
#  Description:
#  This script checks for the presence of Python and Ruby, and executes all 
#  Python and Ruby test files located in the 'test' subdirectory. It displays
#  the paths and versions of Python and Ruby being used, and checks if each
#  test passes or fails, along with skipped test cases.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.9 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.8 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.7 2025-01-13
#       Added display of Python and Ruby versions in both initial setup
#       and final test reports for enhanced visibility.
#  v1.6 2025-01-11
#       Added support for counting skipped test cases in both Python and Ruby.
#  v1.5 2024-03-15
#       Added functionality to display total number of test scripts
#       and test cases executed for each language.
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
#      ./run_tests.sh [Python path] [RSpec path]
#
#  If no paths are specified, it will use the default system paths.
#
########################################################################

# Prevent __pycache__ directory creation
export PYTHONDONTWRITEBYTECODE=1

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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Function to extract test count from Python test result
extract_python_test_count() {
    local result="$1"
    local total_regex='Ran ([0-9]+) tests?'
    local skipped_regex='\(skipped=([0-9]+)\)'

    if [[ $result =~ $total_regex ]]; then
        python_tests="${BASH_REMATCH[1]}"
    else
        python_tests=0
    fi

    if [[ $result =~ $skipped_regex ]]; then
        python_skipped_tests=$((python_skipped_tests + ${BASH_REMATCH[1]}))
    else
        python_skipped_tests=$((python_skipped_tests + 0))
    fi
}

# Function to extract test count from Ruby test result
extract_ruby_test_count() {
    local result="$1"
    local total_regex='([0-9]+) examples?, [0-9]+ failures?'
    local skipped_regex='([0-9]+) pending'

    if [[ $result =~ $total_regex ]]; then
        ruby_tests="${BASH_REMATCH[1]}"
    else
        ruby_tests=0
    fi

    if [[ $result =~ $skipped_regex ]]; then
        ruby_skipped_tests=$((ruby_skipped_tests + ${BASH_REMATCH[1]}))
    else
        ruby_skipped_tests=$((ruby_skipped_tests + 0))
    fi
}

# Function to run Python tests
run_python_tests() {
    # Check if Python is installed
    if [ -z "$python_path" ]; then
        python_path=$(command -v python)
    fi

    if [ -z "$python_path" ]; then
        echo "Python is not installed. Skipping Python tests."
    else
        if [ ! -x "$python_path" ]; then
            echo "Error: Specified Python path is either invalid or not executable." >&2
            exit 1
        fi
        echo "Python path: $python_path"
        python_version=$("$python_path" --version 2>&1)
        echo "$python_version"

        # Execute Python tests
        for file in test/*_test.py; do
            echo "Running Python test: $file"
            output="$("$python_path" "$file" 2>&1)"
            echo "$output"
            if ! echo "$output" | grep -qE "OK|SKIPPED|OK \(skipped=[0-9]+\)" ; then
                echo "Failure in Python test: $file"
                python_failures=$((python_failures + 1))
            fi
            extract_python_test_count "$output"
            python_scripts=$((python_scripts + 1))
            total_tests=$((total_tests + python_tests))
        done
        total_scripts=$((total_scripts + python_scripts))
        display_python_report
    fi
}

# Function to run Ruby tests
run_ruby_tests() {
    # Check if RSpec is installed
    if [ -z "$rspec_path" ]; then
        rspec_path=$(command -v rspec)
    fi

    if [ -z "$rspec_path" ]; then
        echo "RSpec is not installed. Skipping Ruby tests."
    else
        if [ ! -x "$rspec_path" ]; then
            echo "Error: Specified RSpec path is either invalid or not executable." >&2
            exit 1
        fi
        echo "RSpec path: $rspec_path"
        ruby_dir="$(dirname "$rspec_path")"
        ruby_command="$ruby_dir/ruby"
        ruby_version=$("$ruby_command" --version 2>&1)
        echo "$ruby_version"

        # Execute Ruby tests
        for file in test/*_test.rb; do
            echo "Running Ruby test: $file"
            output="$("$rspec_path" "$file" 2>&1)"
            echo "$output"
            if ! echo "$output" | grep -q "0 failures"; then
                echo "Failure in Ruby test: $file"
                ruby_failures=$((ruby_failures + 1))
            fi
            extract_ruby_test_count "$output"
            ruby_scripts=$((ruby_scripts + 1))
            total_tests=$((total_tests + ruby_tests))
        done
        total_scripts=$((total_scripts + ruby_scripts))
        display_ruby_report
    fi
}

# Function to display a summary of Ruby test results
display_python_report() {
    echo "All Python tests completed."
    echo "  Python path: $python_path"
    echo "  $python_version"
    echo "  Total Python test scripts: $python_scripts"
    echo "  Total Python test cases: $total_tests"
    echo "  Skipped Python test cases: $python_skipped_tests"
}

# Function to display a final test report
display_ruby_report() {
    echo "All Ruby tests completed."
    echo "  RSpec path: $rspec_path"
    echo "  $ruby_version"
    echo "  Total Ruby test scripts: $ruby_scripts"
    echo "  Total Ruby test cases: $ruby_tests"
    echo "  Skipped Ruby test cases: $ruby_skipped_tests"
}

# Function to run all tests
display_final_report() {
    # Final report
    total_failures=$((python_failures + ruby_failures))
    if [ "$total_failures" -ne 0 ]; then
        echo "Some tests failed. Total failures: $total_failures." >&2
        exit 1
    else
        echo "All tests passed successfully."
        echo "  Python: $python_version"
        echo "  Ruby: $ruby_version"
        echo "  Total test scripts: $total_scripts"
        echo "  Total test cases: $total_tests"
        echo "  Skipped test cases: $((python_skipped_tests + ruby_skipped_tests))"
        exit 0
    fi
}

# Function to run tests
run_tests() {
    case "$1" in
        -h|--help) usage ;;
    esac

    cd "$SCRIPTS" || exit

    python_path="$1"
    rspec_path="$2"

    # Initialize counters
    python_failures=0
    ruby_failures=0
    python_tests=0
    ruby_tests=0
    python_skipped_tests=0
    ruby_skipped_tests=0
    python_scripts=0
    ruby_scripts=0
    total_tests=0
    total_scripts=0

    run_python_tests
    run_ruby_tests
    display_final_report
}

# Main function to execute the script
main() {
    check_scripts
    run_tests "$@"
}

# Execute main function
main "$@"
