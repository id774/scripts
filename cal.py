#!/usr/bin/env python
#
########################################################################
# Calendar Printer (cal.py)
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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-11-29
#       Updated to pass arguments to system's 'cal' command if provided.
#  v1.0 2023-11-25
#       Initial release. Functionality to print calendars for the current,
#       previous, and next month.
#
# Usage:
#  Run the script with or without arguments:
#      python cal.py [arguments]
#
#  Without arguments, it prints the calendars for the current, previous,
#  and next month. With arguments, it executes the system's 'cal' command
#  with those arguments.
#
########################################################################

import subprocess
import datetime
import calendar
import platform
import sys
import os

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
                cal_lines[i] = cal_lines[i].replace(str(highlight_day), '{}*'.format(highlight_day))
                break

    for line in cal_lines:
        print(line)

def print_cal_month(year, month):
    """
    Print the calendar for a given month and year using the 'cal' command.
    """
    subprocess.call(['cal', str(month), str(year)])

def is_command_exist(command):
    """
    Check if a given command exists in the system.
    """
    return subprocess.call(['which', command], stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

def run_system_cal(args):
    """
    Run the system's 'cal' command with the given arguments.
    Checks if the 'cal' command is different from this script itself to avoid infinite loop.
    """
    cal_path = subprocess.check_output(['which', 'cal']).decode().strip()

    # Get the absolute path of the current script
    script_path = os.path.abspath(__file__)

    # Compare the paths to ensure 'cal' is not this script itself
    if cal_path != script_path:
        subprocess.call(['cal'] + args)
    else:
        print("Error: 'cal' command is the same as this script. Cannot execute to avoid infinite loop.")

def is_unix_like():
    """
    Check if the operating system is Unix-like.
    """
    return platform.system() != "Windows"

# Main execution
if __name__ == '__main__':
    if is_unix_like() and is_command_exist('cal') and len(sys.argv) > 1:
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
            if is_command_exist('cal'):
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

