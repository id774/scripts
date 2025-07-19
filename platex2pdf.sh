#!/bin/sh

########################################################################
# platex2pdf.sh: LaTeX to PDF Conversion Script
#
#  Description:
#  This script converts LaTeX (.tex) files to PDF, handling various
#  encodings and using platex or uplatex depending on the document class.
#  It requires platex, uplatex, dvipdfmx, and nkf to be installed.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./platex2pdf.sh [tex-file]
#
#  Version History:
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-17
#       Encapsulated logic into functions and introduced main function.
#       Added file existence check before processing.
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2024-01-23
#       Integrated check_commands function for dependency checks.
#  v1.1 2023-12-05
#       Refactored for improved readability, added environment checks.
#  v1.0 2014-05-21
#       Initial release. Converts LaTeX files to PDF.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if required commands are available and executable
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

# Detect encoding using nkf
detect_encoding() {
    encoding=$(nkf -g "$1")

    case $encoding in
        UTF-8) echo "-kanji=utf8" ;;
        EUC-JP) echo "-kanji=euc" ;;
        Shift-JIS) echo "-kanji=sjis" ;;
        ISO-2022-JP) echo "-kanji=jis" ;;
        *) echo "" ;;  # Default to an empty string if encoding is not recognized
    esac
}

# Detect which LaTeX engine to use (platex or uplatex)
detect_latex_engine() {
    class=$(sed -n '/documentclass/p' "$1" | sed '/%.*documentclass/d' | sed -n '1p')

    case $class in
        *{u*) echo "uplatex" ;;
        *) echo "platex" ;;
    esac
}

# Convert LaTeX to PDF
convert_to_pdf() {
    if [ ! -f "$1" ]; then
        echo "[ERROR] File '$1' does not exist." >&2
        exit 2
    fi

    dvi_file=$(basename "$1" ".tex")
    kanji_opt=$(detect_encoding "$1")
    latex_engine=$(detect_latex_engine "$1")

    # Run LaTeX command with the detected engine and encoding
    $latex_engine $kanji_opt "$1"
    dvipdfmx "$dvi_file"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Check for input file argument
    if [ -z "$1" ]; then
        usage
    fi

    # Ensure required commands are available
    check_commands platex uplatex dvipdfmx nkf

    convert_to_pdf "$1"
    return 0
}

# Execute main function
main "$@"
exit $?
