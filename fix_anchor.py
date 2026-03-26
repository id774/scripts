#!/usr/bin/env python
# -*- coding: utf-8 -*-

########################################################################
# fix_anchor.py: Normalize reference anchor placement before punctuation
#
#  Description:
#  This script normalizes HTML reference anchors so that anchors linked to
#  "#ref<number>" appear before Japanese full stops.
#
#  It rewrites patterns such as:
#      。<a href="#ref1">[1]</a>
#      。 <a href="#ref1">[1]</a>
#      。<a href="#ref1">[1]</a><a href="#ref2">[2]</a>
#      &#12290;<a href="#ref1">[1]</a>
#
#  into:
#      <a href="#ref1">[1]</a>。
#      <a href="#ref1">[1]</a><a href="#ref2">[2]</a>。
#      <a href="#ref1">[1]</a>&#12290;
#
#  The script updates the input file in place by default, or writes to a
#  separate output file when OUTPUT is specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Requirements:
#  - Python Version: 3.1 or later
#  - Standard library only
#
#  Usage:
#      fix_anchor.py INPUT [OUTPUT]
#      fix_anchor.py -h | --help
#      fix_anchor.py -v | --version
#
#  Options:
#  - INPUT
#      Input HTML file.
#  - OUTPUT
#      Output HTML file. If omitted, INPUT is updated in place.
#  - -h, --help
#      Display this help and exit.
#  - -v, --version
#      Display version information and exit.
#
#  Test Cases:
#      fix_anchor.py sample.html
#      fix_anchor.py sample.html fixed.html
#      fix_anchor.py -h
#      fix_anchor.py -v
#
#  Version History:
#  v1.0 2026-03-26
#       Initial release.
#
########################################################################

import os
import re
import sys

# Match a reference anchor whose href is "#ref<number>".
REF = r'<a\b[^>]*\bhref\s*=\s*["\']#ref\d+["\'][^>]*>\[\d+\]</a>'

# Match consecutive reference anchors.
# Preserve whitespace between anchors as part of the matched block.
REF_RUN = r'%s(?:[ \t\r\n]*%s)*' % (REF, REF)

# Match literal Japanese full stop followed by optional whitespace and refs.
PATTERN = re.compile(
    r'(?P<punct>。)(?P<gap>[ \t\r\n]*)(?P<refs>%s)' % REF_RUN
)

# Match HTML entity form of Japanese full stop followed by refs.
ENTITY_PATTERN = re.compile(
    r'(?P<punct>&#12290;)(?P<gap>[ \t\r\n]*)(?P<refs>%s)' % REF_RUN
)


def usage():
    """Display the script header as usage information and exit."""
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, "r", encoding="utf-8") as f:
            for line in f:
                if line.strip().startswith("#" * 10):
                    if not in_header:
                        in_header = True
                        continue
                    break
                if in_header and line.startswith("#"):
                    if line.startswith("# "):
                        print(line[2:], end="")
                    else:
                        print(line[1:], end="")
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(2)
    sys.exit(0)


def get_script_version():
    """Extract script version from header."""
    script_path = os.path.abspath(__file__)
    found_history = False

    try:
        with open(script_path, 'r', encoding='utf-8') as handle:
            for line in handle:
                if "Version History" in line:
                    found_history = True
                elif found_history and line.strip().startswith("#  v"):
                    return line.strip().split()[1]
    except Exception:
        return "unknown"

    return "unknown"


def show_version():
    """Display version information."""
    version = get_script_version()
    print("fix_anchor.py %s" % version)
    return 0


def validate_input_content(path):
    """Validate that the input file is a supported text file for this script."""

    try:
        with open(path, "rb") as handle:
            raw = handle.read(4096)
    except Exception as exc:
        print("[ERROR] Failed to read input file for validation: %s (%s)" %
              (path, str(exc)), file=sys.stderr)
        return 1

    if b"\x00" in raw:
        print("[ERROR] Binary file is not supported: %s" % path, file=sys.stderr)
        return 1

    try:
        with open(path, "r", encoding="utf-8") as handle:
            text = handle.read()
    except UnicodeDecodeError:
        print("[ERROR] Input file is not valid UTF-8 text: %s" % path,
              file=sys.stderr)
        return 1
    except Exception as exc:
        print("[ERROR] Failed to decode input file: %s (%s)" %
              (path, str(exc)), file=sys.stderr)
        return 1

    if "<a" not in text or "#ref" not in text:
        print("[ERROR] Input file does not look like target HTML text: %s" % path,
              file=sys.stderr)
        return 1

    return 0


def fix_with_pattern(text, pattern):
    """Apply one normalization pattern and return updated text and count."""

    counter = [0]

    def repl(match):
        """Move the reference block before the punctuation."""
        counter[0] += 1
        return "%s%s" % (match.group("refs"), match.group("punct"))

    updated = pattern.sub(repl, text)
    return updated, counter[0]


def fix(text):
    """Normalize reference anchor placement around punctuation."""

    updated, count1 = fix_with_pattern(text, PATTERN)
    updated, count2 = fix_with_pattern(updated, ENTITY_PATTERN)
    return updated, (count1 + count2)


def validate_input_file(path):
    """Validate the input file path."""

    if not os.path.exists(path):
        print("[ERROR] Input file does not exist: %s" % path, file=sys.stderr)
        return 1

    if not os.path.isfile(path):
        print("[ERROR] Input path is not a file: %s" % path, file=sys.stderr)
        return 1

    return 0


def read_text_file(path):
    """Read a UTF-8 text file."""

    try:
        with open(path, "r", encoding="utf-8") as handle:
            return handle.read(), 0
    except Exception as exc:
        print("[ERROR] Failed to read file: %s (%s)" % (path, str(exc)),
              file=sys.stderr)
        return None, 1


def write_text_file(path, text):
    """Write a UTF-8 text file."""

    try:
        with open(path, "w", encoding="utf-8", newline="") as handle:
            handle.write(text)
    except Exception as exc:
        print("[ERROR] Failed to write file: %s (%s)" % (path, str(exc)),
              file=sys.stderr)
        return 1

    return 0


def parse_arguments(argv):
    """Parse command-line arguments."""

    if not argv:
        print("[ERROR] Missing input file", file=sys.stderr)
        return None, None, 1

    if len(argv) == 1:
        arg = argv[0]

        if arg in ("-h", "--help"):
            return "help", None, 0

        if arg in ("-v", "--version"):
            return "version", None, 0

        return arg, arg, 0

    if len(argv) == 2:
        if argv[0] in ("-h", "--help", "-v", "--version"):
            print("[ERROR] Option does not accept an extra argument: %s" % argv[0],
                  file=sys.stderr)
            return None, None, 1

        return argv[0], argv[1], 0

    print("[ERROR] Invalid arguments", file=sys.stderr)
    return None, None, 1


def main():
    """Run the main processing flow."""

    input_path, output_path, status = parse_arguments(sys.argv[1:])
    if status != 0:
        usage()
        return 1

    if input_path == "help":
        return usage()

    if input_path == "version":
        return show_version()

    status = validate_input_file(input_path)
    if status != 0:
        return status

    status = validate_input_content(input_path)
    if status != 0:
        return status

    text, status = read_text_file(input_path)
    if status != 0:
        return status

    updated_text, count = fix(text)

    status = write_text_file(output_path, updated_text)
    if status != 0:
        return status

    print("[INFO] Updated: %d" % count)
    print("[INFO] Written to: %s" % output_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
