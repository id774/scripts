#!/usr/bin/env python

########################################################################
# erase_history.py: Zsh History Tail Line Eraser
#
#  Description:
#  This script deletes recently executed commands from the Zsh history
#  file (~/.zsh_history) in order to quickly remove mistyped or unintended
#  commands recorded in the shell history. When the last history entry
#  corresponds to this script invocation, that entry is preserved and
#  the commands executed immediately before it are removed instead.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      erase_history.py          # removes last 1 command (default)
#      erase_history.py -2
#      erase_history.py -n 2
#      erase_history.py -q
#      erase_history.py -h | --help
#      erase_history.py -v | --version
#
#  Options:
#  - erase_history.py -2
#      Removes the last 2 commands (numeric shorthand).
#  - erase_history.py -n 2
#      Removes the last 2 commands (explicit option).
#  - erase_history.py -q | --quiet
#      Suppress all output.
#
#  Default behavior:
#  - Removes the most recently executed command from ~/.zsh_history.
#  - If the last history entry is this script invocation, the script
#    keeps that entry and removes the command executed immediately
#    before it.
#
#  By default, the deleted lines themselves are printed to standard
#  output exactly as they appeared in the history file. This allows
#  users to visually confirm what will be removed with an explicit info prefix.
#
#  When quiet mode is enabled (-q / --quiet), no output is produced.
#  In quiet mode, the script still performs the deletion but suppresses
#  all standard output.
#
#  Confirmation:
#  - When quiet mode is not enabled, this script prompts for confirmation.
#  - Only "y" or "yes" (case-insensitive) performs deletion.
#
#  Safety:
#  - Maximum removable lines: 10
#
#  Notes:
#  - This script operates strictly on a line basis.
#  - When this script appears as the last history entry, deletion is
#    applied to the preceding commands so that the invocation itself
#    remains visible in history.
#  - The file is updated atomically to avoid history corruption.
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
#  v1.1 2026-02-15
#       Keep this script invocation in history when it is the last entry.
#  v1.0 2026-01-10
#       Initial release.
#
########################################################################

import os
import sys
import tempfile

MAX_REMOVE_LINES = 10


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
    Parse command-line arguments and determine the number of commands to remove.

    Supported patterns:
    - No arguments            -> remove 1 command
    - -<digits> (e.g. -2)     -> remove <digits> commands
    - -n <digits>             -> remove <digits> commands

    Args:
        argv (list): Argument list excluding the script name.

    Returns:
        tuple: (number_of_lines, quiet)

    Exits:
        2: On invalid arguments.
    """

    n = 1
    quiet = False

    if not argv:
        return n, quiet

    if len(argv) == 1:
        a = argv[0]
        if a in ('-h', '--help', '-v', '--version'):
            usage()
        if a in ('-q', '--quiet'):
            quiet = True
            return n, quiet

        if a.startswith('-') and len(a) > 1 and a[1:].isdigit():
            n = int(a[1:], 10)
        else:
            print("[ERROR] Unknown option: %s" % a, file=sys.stderr)
            sys.exit(2)
        return n, quiet

    if len(argv) == 2:
        if argv[0] in ('-n', '--lines'):
            if not argv[1].isdigit():
                print("[ERROR] Invalid number for -n: %s" % argv[1], file=sys.stderr)
                sys.exit(2)
            n = int(argv[1], 10)
            return n, quiet
        if argv[0] in ('-q', '--quiet'):
            quiet = True
            if argv[1].startswith('-') and argv[1][1:].isdigit():
                n = int(argv[1][1:], 10)
                return n, quiet

    if argv and (argv[0] in ('-h', '--help', '-v', '--version')):
        usage()

    print("[ERROR] Invalid arguments", file=sys.stderr)
    sys.exit(2)


def validate_n(n):
    """
    Validate the requested number of commands to remove.

    Args:
        n (int): Number of commands.

    Exits:
        2: If n is less than 1.
    """

    if n <= 0:
        print("[ERROR] N must be >= 1", file=sys.stderr)
        sys.exit(2)
    if n > MAX_REMOVE_LINES:
        print("[ERROR] N must be <= %d" % MAX_REMOVE_LINES, file=sys.stderr)
        sys.exit(2)


def is_self_invocation(history_line):
    """
    Return True if the given history line looks like this script invocation.

    This supports both EXTENDED_HISTORY (": <time>:<dur>;<cmd>") and plain lines.
    """

    s = history_line.rstrip('\n')
    if s.endswith('\r'):
        s = s[:-1]

    if ';' in s:
        s = s.split(';', 1)[1]

    s = s.strip()
    if not s:
        return False

    # Accept common invocations:
    # - erase_history.py ...
    # - ./erase_history.py ...
    # - python erase_history.py ...
    # - python3 /path/to/erase_history.py ...
    toks = s.split()
    if not toks:
        return False

    head = toks[0]
    if head.endswith('/erase_history.py') or head == 'erase_history.py' or head == './erase_history.py':
        return True

    if head in ('python', 'python3'):
        if len(toks) >= 2:
            t1 = toks[1]
            if t1.endswith('/erase_history.py') or t1 == 'erase_history.py' or t1 == './erase_history.py':
                return True

    return False


def erase_tail_lines(history_path, n, quiet):
    """
    Remove the last n lines from the specified history file.

    The function reads the entire file, writes all but the last n lines
    to a temporary file, and then atomically replaces the original file.
    File permissions are preserved when possible.

    Args:
        history_path (str): Path to the Zsh history file.
        n (int): Number of commands to remove.

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

    total = len(lines)
    if total == 0:
        removed_lines = []
        keep_lines = []
    else:
        keep_self = is_self_invocation(lines[-1])
        if keep_self:
            start = total - (n + 1)
            end = total - 1
        else:
            start = total - n
            end = total

        if start < 0:
            start = 0
        if end < 0:
            end = 0

        removed_lines = lines[start:end]
        keep_lines = lines[:start] + lines[end:]

    if not quiet:
        for line in removed_lines:
            # Strip only the trailing newline for consistent single-line output.
            # Preserve the original content as much as possible.
            msg = line.rstrip('\n')
            if msg.endswith('\r'):
                msg = msg[:-1]
            print("[INFO] Will remove line: %s" % msg)

        ans = ""
        try:
            ans = input("Proceed with deletion? [y/yes]: ").strip().lower()
        except EOFError:
            ans = ""

        if ans not in ("y", "yes"):
            print("[INFO] Aborted")
            sys.exit(1)

    dir_name = os.path.dirname(history_path) or '.'
    base_name = os.path.basename(history_path)

    tmp_fd = None
    tmp_path = None
    try:
        tmp_fd, tmp_path = tempfile.mkstemp(
            prefix=base_name + '.', suffix='.tmp', dir=dir_name
        )
        with os.fdopen(tmp_fd, 'w', encoding='utf-8', errors='replace') as w:
            for line in keep_lines:
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

    if not quiet:
        print("[INFO] Deleted")


def main():
    """
    Entry point.

    Parse arguments, validate input, and remove the requested number
    of commands from ~/.zsh_history (default: 1 command).
    """

    n, quiet = parse_args(sys.argv[1:])
    validate_n(n)

    history_path = os.path.expanduser('~/.zsh_history')
    erase_tail_lines(history_path, n, quiet)
    return 0


if __name__ == '__main__':
    if len(sys.argv) >= 2 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())
