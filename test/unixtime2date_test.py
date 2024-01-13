#!/usr/bin/env python

########################################################################
# unixtime2date_test.py: Test script for unixtime2date.py
#
#  Description:
#  Tests the unixtime2date function assuming the local timezone is Japan (UTC+9).
#  Note: These tests are valid only if the system's local timezone is set to Japan.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2023-12-11
#       Initial release.
#
#  Usage:
#  Run this script from the command line using:
#      python test/unixtime2date_test.py
#
########################################################################

import unittest
import sys
import os

# Adjust the path to import unixtime2date from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from unixtime2date import unixtime2date

class TestUnixtime2date(unittest.TestCase):
    """Test cases for unixtime2date function."""

    def test_unixtime2date_japan_timezone(self):
        """Test conversion of Unix timestamps assuming Japan timezone (UTC+9)."""
        # Example Unix timestamp for 2021-01-01 00:00:00 UTC
        timestamp = 1609459200
        # Expected output in Japan timezone (UTC+9)
        expected_date = "2021-01-01T09:00:00+09:00"

        # Test the unixtime2date function
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_leap_year(self):
        # February 29, 2020 (Leap Year) in UTC is February 29 in Japan
        timestamp = 1582934400  # 2020-02-29 00:00:00 UTC
        expected_date = "2020-02-29T09:00:00+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_century_boundary(self):
        # January 1, 2000 (Century Boundary) in UTC is January 1 in Japan
        timestamp = 946684799  # 1999-12-31 23:59:59 UTC
        expected_date = "2000-01-01T08:59:59+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_new_year(self):
        # January 1, 2023 in UTC is January 1 in Japan
        timestamp = 1672531199  # 2022-23-59 00:00:00 UTC
        expected_date = "2023-01-01T08:59:59+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_end_of_year(self):
        # December 31, 2023 in UTC is January 1, 2024 in Japan
        timestamp = 1704067199  # 2023-12-31 23:59:00 UTC
        expected_date = "2024-01-01T08:59:59+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_midnight(self):
        # Midnight (00:00:00) test
        timestamp = 1601510400  # 2020-10-01 00:00:00 UTC
        expected_date = "2020-10-01T09:00:00+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)

    def test_noon(self):
        # Noon (12:00:00) test
        timestamp = 1601553600  # 2020-10-01 12:00:00 UTC
        expected_date = "2020-10-01T21:00:00+09:00"
        self.assertEqual(unixtime2date(timestamp), expected_date)


if __name__ == '__main__':
    unittest.main()
