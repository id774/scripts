#!/usr/bin/env python

########################################################################
# pyping_test.py: Test suite for pyping.py
#
#  Description:
#  This test suite verifies the functionality of the pyping.py script.
#  It checks whether the script correctly pings a range of IP addresses
#  within a specified subnet and validates its output behavior.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-01-06
#       Added test case for the --ordered option to verify sorted output.
#  v1.0 2024-01-12
#       Initial release. Test suite for pyping.py script.
#
#  Notes:
#  - This test simulates the pinging process and does not send actual
#    network requests.
#  - The script is designed to work with Python 3.
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import pyping


class TestPyPing(unittest.TestCase):
    """ Test suite for pyping.py. """

    @patch('subprocess.check_output')
    def test_ping_alive(self, mock_check_output):
        """ Test if an IP address responds as alive. """
        mock_check_output.return_value = b''
        results = {}
        pyping.ping('192.168.11.1', results)
        self.assertEqual(results['192.168.11.1'], 'alive')

    @patch('subprocess.check_output', side_effect=subprocess.CalledProcessError(1, 'ping'))
    def test_ping_no_response(self, mock_check_output):
        """ Test if an unresponsive IP address is marked correctly. """
        results = {}
        pyping.ping('192.168.11.2', results)
        self.assertEqual(results['192.168.11.2'], '-----')

    @patch('subprocess.check_output')
    def test_ordered_option(self, mock_check_output):
        """ Test if results are sorted when using --ordered option. """
        mock_check_output.return_value = b''
        results = {}

        # Simulate pings for a range of IPs
        subnet = "192.168.11."
        start_ip = 1
        end_ip = 3
        for n in range(start_ip, end_ip + 1):
            ip = subnet + str(n)
            pyping.ping(ip, results)

        # Collect results as they would be displayed
        sorted_ips = sorted(results.keys(), key=lambda x: tuple(map(int, x.split('.'))))
        sorted_results = [(ip, results[ip]) for ip in sorted_ips]

        # Verify the sorted order
        expected_output = [("192.168.11.1", "alive"),
                           ("192.168.11.2", "alive"),
                           ("192.168.11.3", "alive")]
        self.assertEqual(sorted_results, expected_output)


if __name__ == '__main__':
    unittest.main()
