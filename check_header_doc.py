#!/usr/bin/env python

########################################################################
# check_header_doc.py: Header Documentation Consistency Checker
#
#  Description:
#  This script scans Git-tracked shell scripts and checks the header
#  documentation block bounded by separator lines (e.g., "########...").
#  It detects missing comment markers inside that header block—specifically,
#  blank lines that should be written as a comment line "#".
#
#  The header block is defined as the content between the first and second
#  separator lines consisting of 20 or more "#" characters. Only that region
#  is inspected; the script does not analyze code or comments outside the
#  header block.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      python check_header_doc.py [options]
#
#  Options:
#      -a, --all-files: Check all Git-tracked files (not only *.sh / shebang "sh").
#      -q, --quiet:     Quiet mode; only prints file:line hits (no header text).
#      -v, --version:   Show version and exit.
#
#  Example:
#      python check_header_doc.py
#
#  Requirements:
#  - Python Version: 3.3 or later
#  - Git (git ls-files)
#
#  Exit Status:
#  - 0: No issues found
#  - 1: Issues found
#  - 2: Fatal error (e.g., cannot run git)
#  - 9: Unsupported Python version
#
#  Version History:
#  v1.0 2026-01-02
#       Initial release. Detect missing "#" for blank lines inside header doc block.
#
########################################################################

import os
import re
import subprocess
import sys
from optparse import OptionParser
from pathlib import Path

SEP_RE = re.compile(r"^#{20,}\s*$")
TARGET_EXTENSIONS = {".sh", ".py", ".rb"}
TARGET_SHEBANGS = ("sh", "bash", "python", "ruby")


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


def run_git_ls_files():
    """Return Git-tracked file paths via `git ls-files`."""
    try:
        out = subprocess.check_output(["git", "ls-files"], text=True, errors="replace")
    except Exception as e:
        raise RuntimeError("git ls-files failed: %s" % str(e))
    return [line.strip() for line in out.splitlines() if line.strip()]


def looks_like_target_script(path, check_all_files):
    """Decide whether the file should be checked (sh / python / ruby)."""
    if check_all_files:
        return True

    p = Path(path)

    if p.suffix in TARGET_EXTENSIONS:
        return True

    try:
        with p.open("r", encoding="utf-8", errors="replace") as f:
            first = f.readline()
    except OSError:
        return False

    if not first.startswith("#!"):
        return False

    return any(lang in first for lang in TARGET_SHEBANGS)


def find_header_bounds(lines):
    """Find start and end indices (inclusive) of the header separator lines."""
    seps = []
    for i, line in enumerate(lines):
        if SEP_RE.match(line):
            seps.append(i)
            if len(seps) >= 2:
                break
    if len(seps) < 2:
        return None
    return (seps[0], seps[1])


def format_hit(path, lineno, msg, quiet_mode, context_line):
    """Format a hit line."""
    if quiet_mode:
        return "%s:%d" % (path, lineno)
    # Keep output single-line and parseable.
    if context_line is None:
        return "%s:%d: %s" % (path, lineno, msg)
    return "%s:%d: %s: %s" % (path, lineno, msg, context_line)


def check_file(path, quiet_mode):
    """
    Check a single file and return list of hits (strings).
    Detect blank lines inside the header block (between first and second separators).
    """
    p = Path(path)
    if not p.is_file():
        return []

    try:
        raw = p.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return []

    lines = raw.splitlines()  # no trailing "\n"
    bounds = find_header_bounds(lines)
    if bounds is None:
        return []

    start, end = bounds
    hits = []

    for idx in range(start + 1, end):
        if lines[idx] == "":
            lineno = idx + 1
            hits.append(
                format_hit(
                    str(p),
                    lineno,
                    "blank line inside header doc (missing '#')",
                    quiet_mode,
                    None,
                )
            )

    return hits


def main():
    """Parse arguments and execute header doc checking."""
    parser = OptionParser("usage: %prog [options]")
    parser.add_option("-v", "--version", help="show the version and exit",
                      action="store_true", dest="version")
    parser.add_option("-a", "--all-files", help="check all git-tracked files",
                      action="store_true", dest="all_files")
    parser.add_option("-q", "--quiet", help="quiet mode (only file:line)",
                      action="store_true", dest="quiet_mode")
    (options, _) = parser.parse_args()

    if options.version:
        print("check_header_doc.py v1.0")
        return 0

    try:
        files = run_git_ls_files()
    except RuntimeError as e:
        print("[ERROR] %s" % str(e), file=sys.stderr)
        return 2

    all_hits = []
    for path in files:
        if not looks_like_target_script(path, options.all_files):
            continue
        all_hits.extend(check_file(path, options.quiet_mode))

    for line in all_hits:
        print(line)

    return 1 if all_hits else 0


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ("-h", "--help", "-v", "--version"):
        usage()

    if sys.version_info < (3, 3):
        print("[ERROR] This script requires Python 3.3 or later.", file=sys.stderr)
        sys.exit(9)

    sys.exit(main())
