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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2024-01-23
#       Integrated check_commands function for dependency checks.
#  v1.1 2023-12-05
#       Refactored for improved readability, added environment checks.
#  v1.0 2014-05-21
#       Initial release. Converts LaTeX files to PDF.
#
#  Usage:
#  ./platex2pdf.sh [tex-file]
#
########################################################################

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check for required commands
check_commands platex uplatex dvipdfmx nkf

# Check for input file
if [ -z "$1" ]; then
    echo "Usage: $0 [tex-file]"
    exit 0
fi

TEX=$*
DVI=`/usr/bin/basename "$TEX" ".tex"`

# Detect encoding
THECODE=`nkf -g "$TEX"`
case $THECODE in
    UTF-8) KANJI="-kanji=utf8";;
    EUC-JP) KANJI="-kanji=euc";;
    Shift-JIS) KANJI="kanji=sjis";;
    ISO-2022-JP) KANJI="-kanji=jis";;
esac

# Determine LaTeX command to use
PLATEX="platex"
CLASS=`sed -n '/documentclass/p' "$TEX" | sed '/%.*documentclass/d' | sed -n '1p'`
case $CLASS in
    *{u*) PLATEX="uplatex";;
esac

# Execute LaTeX and PDF conversion
$PLATEX $KANJI "$TEX"
dvipdfmx "$DVI"

