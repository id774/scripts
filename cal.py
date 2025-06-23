#!/usr/bin/env python

########################################################################
# cal.py: Calendar Printer
#
#  Description:
#  This script acts as a wrapper for the 'cal' command in Unix-like systems.
#  It prints the calendar for the current, previous, and next month if run
#  without arguments. If arguments are provided, it passes them directly to
#  the system's 'cal' command. In non-Unix systems, it uses Python's calendar
#  module for printing.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-05-15
#       Replaced deprecated 'which' usage with POSIX-compliant 'command -v'.
#       Added command_exists() and get_command_path() for command checks and path resolution.
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2023-11-29
#       Updated to pass arguments to system's 'cal' command if provided.
#  v1.0 2023-11-25
#       Initial release. Functionality to print calendars for the current,
#       previous, and next month.
#
#  Usage:
#  Run the script with or without arguments:
#      cal.py [arguments]
#
#  Without arguments, it prints the calendars for the current, previous,
#  and next month. With arguments, it executes the system's 'cal' command
#  with those arguments.
#
########################################################################

import calendar
import datetime
import os
import platform
import subprocess
import sys


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
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
    sys.exit(0)

def print_month_calendar(year, month, highlight_day=None):
    """
    Print the calendar for a given month and year.
    Optionally highlight a specific day.
    """
    cal = calendar.TextCalendar(calendar.SUNDAY)
    cal_lines = cal.formatmonth(year, month).split('\n')

    if highlight_day:
        for i in range(len(cal_lines)):
            if str(highlight_day) in cal_lines[i]:
                cal_lines[i] = cal_lines[i].replace(
                    str(highlight_day), '{}*'.format(highlight_day))
                break

    for line in cal_lines:
        print(line)

def print_cal_month(year, month):
    """
    Print the calendar for a given month and year using the 'cal' command.
    """
    subprocess.call(['cal', str(month), str(year)])

def command_exists(command):
    """
    Checks if a given command exists in the system path using 'command -v'.
    """
    with open(os.devnull, 'w') as devnull:
        return subprocess.call('command -v {}'.format(command), shell=True, stdout=devnull, stderr=devnull) == 0

def get_command_path(command):
    """
    Gets the full path of a command using 'command -v'.
    """
    try:
        with open(os.devnull, 'w') as devnull:
            output = subprocess.check_output('command -v {}'.format(command), shell=True, stderr=devnull)
        return output.decode().strip()
    except subprocess.CalledProcessError:
        return None

def run_system_cal(args):
    """
    Run the system's 'cal' command with the given arguments.
    Checks if the 'cal' command is different from this script itself to avoid infinite loop.
    """
    cal_path = get_command_path('cal')

    # Get the absolute path of the current script
    script_path = os.path.abspath(__file__)

    # Compare the paths to ensure 'cal' is not this script itself
    if cal_path != script_path:
        subprocess.call(['cal'] + args)
    else:
        print("[ERROR] 'cal' command is the same as this script. Cannot execute to avoid infinite loop.", file=sys.stderr)

def is_unix_like():
    """
    Check if the operating system is Unix-like.
    """
    return platform.system() != "Windows"


# Main execution
if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    if is_unix_like() and command_exists('cal') and len(sys.argv) > 1:
        # Pass arguments to the system's 'cal' command
        run_system_cal(sys.argv[1:])
    else:
        # Original script functionality
        now = datetime.datetime.now()
        year = now.year
        month = now.month
        day = now.day

        last_month = (month - 1) if month > 1 else 12
        last_year = year if month > 1 else year - 1
        next_month = (month + 1) if month < 12 else 1
        next_year = year if month < 12 else year + 1

        if is_unix_like():
            if command_exists('cal'):
                print_cal_month(last_year, last_month)
                print_cal_month(year, month)
                print_cal_month(next_year, next_month)
            else:
                print_month_calendar(last_year, last_month)
                print_month_calendar(year, month, day)  # Highlight today
                print_month_calendar(next_year, next_month)
        else:
            print_month_calendar(last_year, last_month)
            print_month_calendar(year, month, day)  # Highlight today
            print_month_calendar(next_year, next_month)
