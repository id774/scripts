#!/usr/bin/env python

########################################################################
# check_header_doc.py: Header Documentation Consistency Checker
#
#  Description:
#  This tool enforces a repository-wide header documentation policy.
#  It scans files under a specified root directory without relying on
#  any VCS metadata, and reports violations in a grep-friendly format
#  suitable for CI and cron-based quality gates.
#
#  This script scans files under a target directory and checks the header
#  documentation block bounded by separator lines (e.g., "########...").
#  It detects missing comment markers inside that header block—specifically,
#  blank lines that should be written as a comment line "#", and typo lines
#  like "##" that should be "#".
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
#      -R, --root:      Root directory to scan (default: current directory).
#      -a, --all-files: Check all files under root (not only sh/python/ruby scripts).
#      -q, --quiet:     Quiet mode; only prints file:line hits (no header text).
#      -v, --version:   Show version and exit.
#
#  Notes:
#  - This script does not depend on Git and works in non-repository directories.
#  - When violations are found in readable files, diagnostic lines are printed.
#  - Intended to be used as a mandatory quality gate in automated test pipelines.
#
#  Example:
#      python check_header_doc.py -a --root /path/to/repo
#
#  Requirements:
#  - Python Version: 3.3 or later
#
#  Exit Status:
#  - 0: No issues found
#  - 1: Issues found
#  - 2: Fatal error (e.g., cannot scan root directory)
#  - 9: Unsupported Python version
#
#  Version History:
#  v1.1 2026-01-10
#       Detect typo blank-comment lines like "##" inside header doc block.
#  v1.0 2026-01-02
#       Initial release. Detect missing "#" for blank lines inside header doc block.
#
########################################################################

import os
import re
import sys
from optparse import OptionParser

SEP_RE = re.compile(r"^#{20,}\s*$")
BAD_BLANK_COMMENT_RE = re.compile(r"^##+\s*$")


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


def looks_like_script(path):
    """Decide whether the file should be checked as a script (sh/python/ruby)."""
    _, ext = os.path.splitext(path)
    if ext in (".sh", ".py", ".rb"):
        return True

    try:
        with open(path, "r", encoding="utf-8", errors="replace") as f:
            first = f.readline()
    except OSError:
        return False

    if not first.startswith("#!"):
        return False

    # Keep this conservative.
    return ("sh" in first) or ("python" in first) or ("ruby" in first)


def iter_files(root_dir, check_all_files):
    """Yield file paths under root_dir (no VCS dependency)."""
    for dirpath, dirnames, filenames in os.walk(root_dir):
        # Skip common noise directories
        dirnames[:] = [d for d in dirnames if d not in (".git", ".svn", ".hg", "__pycache__", ".venv", "venv")]
        for name in filenames:
            path = os.path.join(dirpath, name)
            if check_all_files:
                yield path
            else:
                if looks_like_script(path):
                    yield path


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
    if not os.path.isfile(path):
        return []

    try:
        with open(path, "r", encoding="utf-8", errors="replace") as f:
            raw = f.read()
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
                    path,
                    lineno,
                    "blank line inside header doc (missing '#')",
                    quiet_mode,
                    None,
                )
            )
            continue

        # Detect typo blank-comment lines like "##" (should be "#").
        if BAD_BLANK_COMMENT_RE.match(lines[idx]):
            lineno = idx + 1
            hits.append(
                format_hit(
                    path,
                    lineno,
                    "invalid blank comment line inside header doc (expected '#')",
                    quiet_mode,
                    lines[idx],
                )
            )

    return hits


def main():
    """Parse arguments and execute header doc checking."""
    parser = OptionParser("usage: %prog [options]")
    parser.add_option("-v", "--version", help="show the version and exit",
                      action="store_true", dest="version")
    parser.add_option("-R", "--root", help="root directory to scan",
                      action="store", type="string", dest="root_dir")
    parser.add_option("-a", "--all-files", help="check all files",
                      action="store_true", dest="all_files")
    parser.add_option("-q", "--quiet", help="quiet mode (only file:line)",
                      action="store_true", dest="quiet_mode")
    (options, _) = parser.parse_args()

    if options.version:
        print("check_header_doc.py v1.0")
        return 0

    root_dir = options.root_dir or os.getcwd()
    if not os.path.isdir(root_dir):
        print("[ERROR] root directory not found: %s" % root_dir, file=sys.stderr)
        return 2

    all_hits = []
    for path in iter_files(root_dir, options.all_files):
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
