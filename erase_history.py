#!/usr/bin/env python

########################################################################
# erase_history.py: Zsh History Tail Line Eraser
#
#  Description:
#  This script deletes the most recent lines from the Zsh history file
#  (~/.zsh_history) in order to quickly remove mistyped or unintended
#  commands recorded in the shell history.
#
#  The tool operates strictly on a line basis and performs an atomic
#  file replacement to avoid corrupting the history file even if an
#  error occurs during processing.
#
#  Default behavior:
#  - Removes the last 1 line from ~/.zsh_history.
#
#  Options:
#  - erase_history.py -2
#      Removes the last 2 lines (numeric shorthand).
#  - erase_history.py -n 2
#      Removes the last 2 lines (explicit option).
#
#  Notes:
#  - This script does not attempt to interpret Zsh history entry
#    boundaries; it simply truncates by line count.
#  - The script is designed for minimal latency and minimal side effects.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      erase_history.py
#      erase_history.py -2
#      erase_history.py -n 2
#      erase_history.py -h | --help
#      erase_history.py -v | --version
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
#  v1.0 2026-01-10
#       Initial release.
#
########################################################################

import os
import sys
import tempfile


def usage():
    """
    Display the script header as usage and exit.

    This function extracts the header comment block from this script
    itself and prints it verbatim, ensuring that the usage output
    always matches the documented behavior.
    """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip().startswith('#' * 10):
                    if not in_header:
                        in_header = True
                        continue
                    else:
                        break
                if in_header and line.startswith('#'):
                    if line.startswith('# '):
                        print(line[2:], end='')
                    else:
                        print(line[1:], end='')
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)


def parse_args(argv):
    """
    Parse command-line arguments and determine the number of lines to remove.

    Supported patterns:
    - No arguments            -> remove 1 line
    - -<digits> (e.g. -2)     -> remove <digits> lines
    - -n <digits>             -> remove <digits> lines

    Args:
        argv (list): Argument list excluding the script name.

    Returns:
        int: Number of lines to remove.

    Exits:
        2: On invalid arguments.
    """
    n = 1

    if not argv:
        return n

    if len(argv) == 1:
        a = argv[0]
        if a in ('-h', '--help', '-v', '--version'):
            usage()
        if a.startswith('-') and len(a) > 1 and a[1:].isdigit():
            n = int(a[1:], 10)
        else:
            print("[ERROR] Unknown option: %s" % a, file=sys.stderr)
            sys.exit(2)
        return n

    if len(argv) == 2:
        if argv[0] in ('-n', '--lines'):
            if not argv[1].isdigit():
                print("[ERROR] Invalid number for -n: %s" % argv[1], file=sys.stderr)
                sys.exit(2)
            n = int(argv[1], 10)
            return n

    if argv and (argv[0] in ('-h', '--help', '-v', '--version')):
        usage()

    print("[ERROR] Invalid arguments", file=sys.stderr)
    sys.exit(2)


def validate_n(n):
    """
    Validate the requested number of lines to remove.

    Args:
        n (int): Number of lines.

    Exits:
        2: If n is less than 1.
    """
    if n <= 0:
        print("[ERROR] N must be >= 1", file=sys.stderr)
        sys.exit(2)


def erase_tail_lines(history_path, n):
    """
    Remove the last n lines from the specified history file.

    The function reads the entire file, writes all but the last n lines
    to a temporary file, and then atomically replaces the original file.
    File permissions are preserved when possible.

    Args:
        history_path (str): Path to the Zsh history file.
        n (int): Number of lines to remove.

    Exits:
        1: On read/write failure.
        2: If the history file does not exist.
    """
    if not os.path.exists(history_path):
        print("[ERROR] History file does not exist - %s" % history_path, file=sys.stderr)
        sys.exit(2)

    try:
        with open(history_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()
    except Exception as e:
        print("[ERROR] Failed to read history file - %s (%s)" %
              (history_path, str(e)), file=sys.stderr)
        sys.exit(1)

    keep = len(lines) - n
    if keep < 0:
        keep = 0

    dir_name = os.path.dirname(history_path) or '.'
    base_name = os.path.basename(history_path)

    tmp_fd = None
    tmp_path = None
    try:
        tmp_fd, tmp_path = tempfile.mkstemp(
            prefix=base_name + '.', suffix='.tmp', dir=dir_name
        )
        with os.fdopen(tmp_fd, 'w', encoding='utf-8', errors='replace') as w:
            for line in lines[:keep]:
                w.write(line)

        try:
            st = os.stat(history_path)
            os.chmod(tmp_path, st.st_mode)
        except Exception:
            pass

        os.replace(tmp_path, history_path)
        tmp_path = None
    except Exception as e:
        if tmp_fd is not None:
            try:
                os.close(tmp_fd)
            except Exception:
                pass
        if tmp_path:
            try:
                os.unlink(tmp_path)
            except Exception:
                pass
        print("[ERROR] Failed to update history file - %s (%s)" %
              (history_path, str(e)), file=sys.stderr)
        sys.exit(1)


def main():
    """
    Entry point.

    Parse arguments, validate input, and remove the requested number
    of lines from ~/.zsh_history.
    """
    n = parse_args(sys.argv[1:])
    validate_n(n)

    history_path = os.path.expanduser('~/.zsh_history')
    erase_tail_lines(history_path, n)
    return 0


if __name__ == '__main__':
    if len(sys.argv) >= 2 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
