#!/usr/bin/env python

########################################################################
# pyping_test.py: Test suite for pyping.py
#
#  Description:
#  This test suite verifies the functionality of the pyping.py script.
#  It checks whether the script correctly pings a range of IP addresses
#  within a specified subnet.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
    """Test suite for pyping.py."""

    @patch('subprocess.check_output')
    def test_ping_alive(self, mock_check_output):
        """Test if an IP address responds as alive."""
        mock_check_output.return_value = b''
        results = {}
        pyping.ping('192.168.11.1', results)
        self.assertEqual(results['192.168.11.1'], 'alive')

    @patch('subprocess.check_output', side_effect=subprocess.CalledProcessError(1, 'ping'))
    def test_ping_no_response(self, mock_check_output):
        """Test if an unresponsive IP address is marked correctly."""
        results = {}
        pyping.ping('192.168.11.2', results)
        self.assertEqual(results['192.168.11.2'], '-----')


if __name__ == '__main__':
    unittest.main()
