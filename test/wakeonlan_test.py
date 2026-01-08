#!/usr/bin/env python

########################################################################
# wakeonlan_test.py: Test script for wakeonlan.py
#
#  Description:
#  This test script contains unit tests for the wakeonlan.py script.
#  It verifies the functionality of creating and formatting magic packets,
#  as well as sending them over the network without actually doing so.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Test Cases:
#    - Verifies that the script prints usage and exits with code 0 when invoked with the -h option.
#    - Formats MAC addresses correctly by removing separators and normalizing output.
#    - Creates a valid Wake-on-LAN magic packet from a given MAC address.
#    - Sends a magic packet without performing actual network communication (mocked UDP broadcast).
#
#  Version History:
#  v1.1 2024-02-26
#       Modified test cases to suppress message output during tests.
#  v1.0 2024-02-10
#       Initial test script for wakeonlan.py
#
########################################################################

import os
import subprocess
import sys
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from wakeonlan import (create_magic_packet, format_mac_address,
                       send_magic_packet, send_udp_broadcast)


class TestWakeOnLan(unittest.TestCase):
    """ Unit tests for the wakeonlan.py script. """

    def test_usage_shows_help(self):
        script_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        script_path = os.path.join(script_dir, 'wakeonlan.py')

        proc = subprocess.Popen(['python3', script_path, '-h'],
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
        out, err = proc.communicate()

        self.assertEqual(proc.returncode, 0)
        self.assertIn('Usage:', out.decode('utf-8'))

    def test_format_mac_address(self):
        """ Test formatting MAC addresses. """
        formatted_mac = format_mac_address("00-11-22-33-44-55")
        self.assertEqual(formatted_mac, "001122334455")

    def test_create_magic_packet(self):
        """ Test creating a magic packet from a MAC address. """
        mac = "001122334455"
        packet = create_magic_packet(mac)
        expected_packet = bytes.fromhex('FF' * 6 + mac * 16)
        self.assertEqual(packet, expected_packet)

    @patch('builtins.print')
    @patch('wakeonlan.send_udp_broadcast')
    def test_send_magic_packet(self, mock_send_udp_broadcast, mock_print):
        """ Test sending a magic packet without actual network communication. """
        mac = "00:11:22:33:44:55"
        send_magic_packet(mac)
        mock_send_udp_broadcast.assert_called_once()


if __name__ == '__main__':
    unittest.main()
