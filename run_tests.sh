#!/bin/sh

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
#  Usage:
#  Run this script from the command line to execute all tests:
#      ./run_tests.sh [Python path] [RSpec path]
#
#  If no paths are specified, it will use the default system paths.
#
#  Version History:
#  v3.2 2025-07-09
#       Refactor Ruby test handling to check ruby before rspec and require matching pair to avoid fallback.
#       Enforce validation of explicitly passed python path and fallback only when unspecified.
#  v3.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#       Fix bug where Ruby test case count only reflected last script instead of total.
#  v3.0 2025-04-29
#       Fully POSIX-compliant rewrite. Removed Bash-specific syntax and structures.
#  v2.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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
########################################################################

# Prevent __pycache__ directory creation
export PYTHONDONTWRITEBYTECODE=1

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the SCRIPTS variable is unset or empty
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Extract test count from Python test result
extract_python_test_count() {
    result="$1"

    python_tests=0
    python_skipped_tests_in_test=0

    if echo "$result" | grep -q "Ran [0-9][0-9]* test"; then
        python_tests=`echo "$result" | awk '/Ran [0-9]+/ {for (i=1;i<=NF;i++) if ($i ~ /^[0-9]+$/) {print $i; exit}}'`
    fi

    python_skipped_tests_in_test=`echo "$result" | awk 'match($0, /\(skipped=[0-9]+\)/) {print substr($0, RSTART+9, RLENGTH-10); exit}'`
    python_skipped_tests_in_test=${python_skipped_tests_in_test:-0}
    python_skipped_tests=`expr "$python_skipped_tests" + "$python_skipped_tests_in_test"`
}

# Extract test count from Ruby test result
extract_ruby_test_count() {
    result="$1"

    ruby_tests=0
    ruby_skipped_tests_in_test=0

    if echo "$result" | grep -q "[0-9][0-9]* examples\?, [0-9][0-9]* failures\?"; then
        ruby_tests=`echo "$result" | awk '{for (i=1;i<=NF;i++) if ($i ~ /^[0-9]+$/) {print $i; exit}}'`
    fi

    if echo "$result" | grep -q "[0-9][0-9]* pending"; then
        ruby_skipped_tests_in_test=`echo "$result" | awk '{for (i=1;i<=NF;i++) if ($i ~ /^[0-9]+$/ && $(i+1) == "pending") {print $i; exit}}'`
    fi
    ruby_skipped_tests_in_test=${ruby_skipped_tests_in_test:-0}
    ruby_skipped_tests=`expr "$ruby_skipped_tests" + "$ruby_skipped_tests_in_test"`
}

# Run Python tests
run_python_tests() {
    # Flag whether python_path was explicitly passed
    python_path_explicit=0
    if [ -n "$python_path" ]; then
        python_path_explicit=1
    fi

    # Fallback to system python if not explicitly set
    if [ -z "$python_path" ]; then
        python_path=$(command -v python)
    fi

    # Validate python_path executable
    if [ ! -x "$python_path" ]; then
        if [ "$python_path_explicit" -eq 1 ]; then
            echo "[ERROR] Specified Python path is invalid or not executable: $python_path" >&2
            exit 1
        else
            echo "[ERROR] Python not found or not executable: $python_path" >&2
            echo "[INFO] Use './run_tests.sh /full/path/to/python' to specify Python explicitly." >&2
            exit 1
        fi
    fi

    echo "[INFO] Python path: $python_path"
    python_version=$("$python_path" --version 2>&1)
    echo "$python_version"

    # Execute Python test scripts
    for file in test/*_test.py; do
        if [ -f "$file" ]; then
            echo "[INFO] Running Python test: $file"
            output=$("$python_path" "$file" 2>&1)
            echo "$output"
            if ! echo "$output" | grep -qE "OK|SKIPPED|OK (skipped=[0-9]+)"; then
                echo "[WARN] Failure in Python test: $file" >&2
                python_failures=$((python_failures + 1))
            fi
            extract_python_test_count "$output"
            python_scripts=$((python_scripts + 1))
            total_tests=$((total_tests + python_tests))
        fi
    done

    total_scripts=$((total_scripts + python_scripts))
    display_python_report
}

# Run Ruby tests
run_ruby_tests() {
    # Check if Ruby is installed
    if [ -z "$ruby_path" ]; then
        # Infer from rspec path if available
        if [ -n "$rspec_path" ]; then
            ruby_candidate="$(dirname "$rspec_path")/ruby"
            if [ -x "$ruby_candidate" ]; then
                ruby_path="$ruby_candidate"
            else
                echo "[ERROR] ruby not found next to specified rspec: $rspec_path" >&2
                exit 1
            fi
        else
            ruby_path=$(command -v ruby)
        fi
    fi

    if [ -z "$ruby_path" ]; then
        echo "[INFO] Ruby is not installed. Skipping Ruby tests." >&2
        return
    elif [ ! -x "$ruby_path" ]; then
        echo "[ERROR] Specified Ruby path is invalid or not executable." >&2
        exit 1
    fi

    # Check if RSpec is installed
    if [ -z "$rspec_path" ]; then
        rspec_path=$(command -v rspec)
    fi

    if [ -z "$rspec_path" ]; then
        echo "[INFO] Ruby is installed, but RSpec is not available. Skipping Ruby tests." >&2
        return
    elif [ ! -x "$rspec_path" ]; then
        echo "[ERROR] Specified RSpec path is invalid or not executable." >&2
        exit 1
    fi

    echo "[INFO] Ruby path: $ruby_path"
    ruby_version=$("$ruby_path" --version 2>&1)
    echo "$ruby_version"

    echo "[INFO] RSpec path: $rspec_path"

    # Execute Ruby tests
    for file in test/*_test.rb; do
        if [ -f "$file" ]; then
            echo "[INFO] Running Ruby test: $file"
            output=$("$rspec_path" "$file" 2>&1)
            echo "$output"
            if ! echo "$output" | grep -q "0 failures"; then
                echo "[WARN] Failure in Ruby test: $file" >&2
                ruby_failures=$(expr "$ruby_failures" + 1)
            fi
            extract_ruby_test_count "$output"
            ruby_scripts=$(expr "$ruby_scripts" + 1)
            ruby_tests_total=$(expr "$ruby_tests_total" + "$ruby_tests")
            total_tests=$(expr "$total_tests" + "$ruby_tests")
        fi
    done

    total_scripts=$(expr "$total_scripts" + "$ruby_scripts")
    display_ruby_report
}

# Display Python report
display_python_report() {
    echo "[INFO] All Python tests completed."
    echo "  Python path: $python_path"
    echo "  $python_version"
    echo "  Total Python test scripts: $python_scripts"
    echo "  Total Python test cases: $total_tests"
    echo "  Skipped Python test cases: $python_skipped_tests"
}

# Display Ruby report
display_ruby_report() {
    echo "[INFO] All Ruby tests completed."
    echo "  RSpec path: $rspec_path"
    echo "  $ruby_version"
    echo "  Total Ruby test scripts: $ruby_scripts"
    echo "  Total Ruby test cases: $ruby_tests_total"
    echo "  Skipped Ruby test cases: $ruby_skipped_tests"
}

# Display final report
display_final_report() {
    total_failures=`expr "$python_failures" + "$ruby_failures"`
    if [ "$total_failures" -ne 0 ]; then
        echo "[WARN] Some tests failed. Total failures: $total_failures." >&2
        exit 1
    else
        echo "[INFO] All tests passed successfully."

        if [ "$python_scripts" -gt 0 ] && [ "$ruby_scripts" -gt 0 ]; then
            echo "  Python: $python_version"
            echo "  Ruby: $ruby_version"
            echo "  Total test scripts: $total_scripts"
            echo "  Total test cases: $total_tests"
            echo "  Skipped test cases: `expr "$python_skipped_tests" + "$ruby_skipped_tests"`"
        fi

        exit 0
    fi
}

# Run all tests
run_tests() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
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
    ruby_tests_total=0

    run_python_tests
    run_ruby_tests
    display_final_report
}

# Main entry point of the script
main() {
    check_scripts
    run_tests "$@"
    return 0
}

# Execute main function
main "$@"
exit $?
