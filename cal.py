#!/usr/bin/env python
#
########################################################################
# Calendar Printer
#
#  Description:
#  This script prints the calendar for the current, previous, and next month.
#  It checks the operating system and uses the 'cal' command in Unix-like systems,
#  or the Python 'calendar' module otherwise. It highlights the current day in the
#  current month's calendar.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 11/25,2023
#       Initial release. Includes functionality to check the operating system,
#       verify if 'cal' command exists, and print calendars for the current,
#       previous, and next month.
#
# Usage:
#  Run the script without any arguments:
#      python calendar_printer.py
#
#  The script automatically determines the current date and prints
#  the calendars for the current, previous, and next month.
#  If running on a Unix-like system with the 'cal' command,
#  it will use 'cal' for printing. Otherwise, it will use
#  Python's calendar module.
#
########################################################################

import subprocess
import datetime
import calendar
import platform

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

def is_unix_like():
    """
    Check if the operating system is Unix-like.
    """
    return platform.system() != "Windows"

# Get the current year, month, and day
now = datetime.datetime.now()
year = now.year
month = now.month
day = now.day

# Calculate last month and next month
last_month = (month - 1) if month > 1 else 12
last_year = year if month > 1 else year - 1
next_month = (month + 1) if month < 12 else 1
next_year = year if month < 12 else year + 1

# Check the platform and print the calendars accordingly
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

