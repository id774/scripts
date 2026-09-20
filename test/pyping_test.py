#!/usr/bin/env python

########################################################################
# pyping_test.py: Test suite for pyping.py
#
#  Description:
#  This test suite verifies the functionality of the pyping.py script.
#  It checks whether the script correctly pings a range of IP addresses
#  within a specified subnet and validates its output behavior.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with -h option.
#    - Mark an IP address as "alive" when ping returns successfully.
#    - Mark an IP address as "-----" when ping returns no response (CalledProcessError).
#    - Record an OSError in the errors dict, without a host-down result, when ping cannot be executed.
#    - Reject invalid IP ranges before launching worker threads.
#    - Sort ping results in numeric IP order when the --ordered option behavior is used.
#    - Return 127 and start no worker threads when the ping command is missing.
#    - Return 126 and start no worker threads when the ping command is not executable.
#    - Return 1 from main() when at least one worker reports a ping execution error.
#    - Return 0 from main() when only unreachable hosts are reported.
#
#  Notes:
#    - This test simulates the pinging process and does not send actual network requests.
#    - The script is designed to work with Python 3.
#
#  Version History:
#  v1.2 2026-09-20
#       Cover range validation, unreachable hosts, and ping execution failures.
#  v1.1 2025-01-06
#       Added test case for the --ordered option to verify sorted output.
#  v1.0 2024-01-12
#       Initial release.
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

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'pyping.py')

        proc = subprocess.Popen(['python', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    @patch('subprocess.check_output')
    def test_ping_alive(self, mock_check_output):
        """ Test if an IP address responds as alive. """
        mock_check_output.return_value = b''
        results = {}
        errors = {}
        pyping.ping('192.168.11.1', results, errors)
        self.assertEqual(results['192.168.11.1'], 'alive')
        self.assertEqual(errors, {})

    @patch('subprocess.check_output', side_effect=subprocess.CalledProcessError(1, 'ping'))
    def test_ping_no_response(self, mock_check_output):
        """ Test if an unresponsive IP address is marked correctly. """
        results = {}
        errors = {}
        pyping.ping('192.168.11.2', results, errors)
        self.assertEqual(results['192.168.11.2'], '-----')
        self.assertEqual(errors, {})

    @patch('subprocess.check_output', side_effect=FileNotFoundError('ping'))
    def test_ping_command_unavailable(self, mock_check_output):
        """ Test if a ping execution error is recorded without a host-down result. """
        results = {}
        errors = {}
        pyping.ping('192.168.11.3', results, errors)
        self.assertNotIn('192.168.11.3', results)
        self.assertIn('192.168.11.3', errors)

    def test_validate_range_rejects_reversed_range(self):
        """ Test if start_ip greater than end_ip is rejected. """
        with patch('sys.stderr'):
            self.assertFalse(pyping.validate_range(10, 1))

    def test_validate_range_rejects_out_of_bounds_range(self):
        """ Test if host-number values outside 0-255 are rejected. """
        with patch('sys.stderr'):
            self.assertFalse(pyping.validate_range(-1, 256))

    def test_validate_range_accepts_valid_range(self):
        """ Test if a valid host-number range is accepted. """
        self.assertTrue(pyping.validate_range(1, 254))

    @patch('subprocess.check_output')
    def test_ordered_option(self, mock_check_output):
        """ Test if results are sorted when using --ordered option. """
        mock_check_output.return_value = b''
        results = {}
        errors = {}

        # Simulate pings for a range of IPs
        subnet = "192.168.11."
        start_ip = 1
        end_ip = 3
        for n in range(start_ip, end_ip + 1):
            ip = subnet + str(n)
            pyping.ping(ip, results, errors)

        # Collect results as they would be displayed
        sorted_ips = sorted(results.keys(), key=lambda x: tuple(map(int, x.split('.'))))
        sorted_results = [(ip, results[ip]) for ip in sorted_ips]

        # Verify the sorted order
        expected_output = [("192.168.11.1", "alive"),
                           ("192.168.11.2", "alive"),
                           ("192.168.11.3", "alive")]
        self.assertEqual(sorted_results, expected_output)

    @patch('pyping.find_command_with_status', return_value=(None, 127))
    @patch('pyping.threading.Thread')
    def test_main_returns_127_when_ping_missing(self, mock_thread, mock_find_command):
        """ Test that main() returns 127 and starts no worker threads when ping is missing. """
        with patch('sys.stderr'):
            rc = pyping.main("192.168.11.", 1, 2, False)
        self.assertEqual(rc, 127)
        mock_thread.assert_not_called()

    @patch('pyping.find_command_with_status', return_value=(None, 126))
    @patch('pyping.threading.Thread')
    def test_main_returns_126_when_ping_not_executable(self, mock_thread, mock_find_command):
        """ Test that main() returns 126 and starts no worker threads when ping is not executable. """
        with patch('sys.stderr'):
            rc = pyping.main("192.168.11.", 1, 2, False)
        self.assertEqual(rc, 126)
        mock_thread.assert_not_called()

    @patch('pyping.find_command_with_status', return_value=('/bin/ping', 0))
    @patch('subprocess.check_output', side_effect=OSError('boom'))
    def test_main_returns_1_on_worker_execution_error(self, mock_check_output, mock_find_command):
        """ Test that main() returns 1 when at least one worker reports an execution error. """
        with patch('sys.stderr'):
            rc = pyping.main("192.168.11.", 1, 2, False)
        self.assertEqual(rc, 1)

    @patch('pyping.find_command_with_status', return_value=('/bin/ping', 0))
    @patch('subprocess.check_output', side_effect=subprocess.CalledProcessError(1, 'ping'))
    def test_main_returns_0_for_unreachable_hosts_only(self, mock_check_output, mock_find_command):
        """ Test that main() returns 0 when only unreachable hosts are reported. """
        rc = pyping.main("192.168.11.", 1, 2, False)
        self.assertEqual(rc, 0)


if __name__ == '__main__':
    unittest.main()
