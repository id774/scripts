#!/usr/bin/env python

import calendar
import datetime

def print_month_calendar(year, month, highlight_day=None):
    """
    Print the calendar for a given month and year.
    Optionally highlight a specific day.
    """
    cal = calendar.TextCalendar(calendar.SUNDAY)
    # Generate the month's calendar as a list of weeks
    cal_lines = cal.formatmonth(year, month).split('\n')
    
    # Highlight the specified day if provided
    if highlight_day:
        for i in range(len(cal_lines)):
            if str(highlight_day) in cal_lines[i]:
                cal_lines[i] = cal_lines[i].replace(str(highlight_day), '{}*'.format(highlight_day))
                break

    # Print the formatted calendar
    for line in cal_lines:
        print(line)

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

# Print the calendars
print_month_calendar(last_year, last_month)
print_month_calendar(year, month, day)  # Highlight today
print_month_calendar(next_year, next_month)

