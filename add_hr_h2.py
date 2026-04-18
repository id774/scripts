#!/usr/bin/env python
# -*- coding: utf-8 -*-

########################################################################
# add_hr_h2.py: Insert <hr> immediately before <h2> tags
#
#  Description:
#  This script inserts an <hr> tag immediately before each <h2> tag in
#  an HTML file, unless an <hr> tag is already present just before that
#  <h2> block.
#
#  It rewrites patterns such as:
#      <p>text</p>
#
#      <h2>title</h2>
#
#  into:
#      <p>text</p>
#
#      <hr>
#      <h2>title</h2>
#
#  It does not modify cases such as:
#      <p>text</p>
#
#      <hr>
#      <h2>title</h2>
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
#      add_hr_h2.py INPUT [OUTPUT]
#      add_hr_h2.py -h | --help
#      add_hr_h2.py -v | --version
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
#  Version History:
#  v1.0 2026-04-18
#       Initial release.
#
########################################################################

import os
import re
import sys

# Match an opening <h2> tag with optional attributes.
H2_TAG_PATTERN = re.compile(r'<h2\b[^>]*>', re.IGNORECASE)

# Match an <hr> tag at the end of a text fragment.
HR_AT_END_PATTERN = re.compile(r'<hr>\s*$', re.IGNORECASE)


def usage():
    """Display the script header as usage information and exit."""

    script_path = os.path.abspath(__file__)
    in_header = False

    try:
        with open(script_path, "r", encoding="utf-8") as handle:
            for line in handle:
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
    except Exception as exc:
        print("Error reading usage information: %s" % str(exc), file=sys.stderr)
        sys.exit(1)

    sys.exit(0)


def get_script_version():
    """Extract the first version entry from the script header."""

    script_path = os.path.abspath(__file__)
    found_history = False

    try:
        with open(script_path, "r", encoding="utf-8") as handle:
            for line in handle:
                if "Version History" in line:
                    found_history = True
                    continue

                if found_history and line.strip().startswith("#  v"):
                    parts = line.strip().split()
                    if len(parts) >= 2:
                        return parts[1]
    except Exception:
        return "unknown"

    return "unknown"


def show_version():
    """Display version information."""

    version = get_script_version()
    print("add_hr_h2.py %s" % version)
    return 0


def validate_input_file(path):
    """Validate that the input path exists and is a regular file."""

    if not os.path.exists(path):
        print("[ERROR] Input file does not exist: %s" % path, file=sys.stderr)
        return 1

    if not os.path.isfile(path):
        print("[ERROR] Input path is not a file: %s" % path, file=sys.stderr)
        return 1

    return 0


def validate_input_content(path):
    """Validate that the input file is readable UTF-8 HTML-like text."""

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

    if "<h2" not in text.lower():
        print("[ERROR] Input file does not contain any <h2> tag: %s" % path,
              file=sys.stderr)
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
    """Write text to a UTF-8 file without newline translation."""

    try:
        with open(path, "w", encoding="utf-8", newline="") as handle:
            handle.write(text)
    except Exception as exc:
        print("[ERROR] Failed to write file: %s (%s)" % (path, str(exc)),
              file=sys.stderr)
        return 1

    return 0


def has_hr_immediately_before(text, h2_start):
    """Check whether an <hr> tag already exists immediately before <h2>."""

    prefix = text[:h2_start]
    stripped = prefix.rstrip()
    return HR_AT_END_PATTERN.search(stripped) is not None


def detect_newline(text):
    """Detect the dominant newline style used in the input text."""

    if "\r\n" in text:
        return "\r\n"

    if "\r" in text:
        return "\r"

    return "\n"


def add_hr_before_h2(text):
    """Insert <hr> directly before each <h2> tag when needed."""

    newline = detect_newline(text)
    parts = []
    position = 0
    inserted_count = 0

    for match in H2_TAG_PATTERN.finditer(text):
        h2_start = match.start()

        if has_hr_immediately_before(text, h2_start):
            continue

        line_start = text.rfind(newline, 0, h2_start)
        if line_start == -1:
            line_start = 0
        else:
            line_start += len(newline)

        indent = text[line_start:h2_start]
        if not re.match(r'^[ \t]*$', indent):
            indent = ""

        parts.append(text[position:line_start])
        parts.append(indent + "<hr>" + newline)
        position = line_start
        inserted_count += 1

    parts.append(text[position:])

    return "".join(parts), inserted_count


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

    updated_text, count = add_hr_before_h2(text)

    status = write_text_file(output_path, updated_text)
    if status != 0:
        return status

    print("[INFO] Inserted <hr>: %d" % count)
    print("[INFO] Written to: %s" % output_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
