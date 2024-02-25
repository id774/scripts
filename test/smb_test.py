#!/usr/bin/env python

########################################################################
# smb_test.py: Test script for smb.py
#
#  Description:
#  Tests the functionality of the smb.py script, ensuring correct mount
#  command generation for both Windows and POSIX systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-01-06
#       Initial release of test script.
#
#  Usage:
#  Run this script from the command line using:
#      python test/smb_test.py
#
########################################################################

import logging
import os
import sys
import unittest
from unittest.mock import patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import smb


class TestExecOnWin(unittest.TestCase):
    """Test cases for ExecOnWin class in smb.py."""

    @patch('os.system', return_value=0)
    @patch('subprocess.getoutput', return_value="")
    def test_run_win(self, mock_getoutput, mock_system):
        """Test the command generation for Windows system."""
        smb.logger.setLevel(logging.CRITICAL)
        executor = smb.ExecOnWin()
        executor.run("Z:", "sharename1", "192.168.21.12",
                     "username1", "password1")
        mock_getoutput.assert_called_with(
            "net use Z: \\\\\\\\192.168.21.12\\\\sharename1 password1 /USER:username1 /persistent:no"
        )

class TestExecOnPosix(unittest.TestCase):
    """Test cases for ExecOnPosix class in smb.py."""

    @patch('os.system', return_value=0)
    @patch('subprocess.getoutput', return_value="")
    def test_run_posix(self, mock_getoutput, mock_system):
        """Test the command generation for POSIX system."""
        smb.logger.setLevel(logging.CRITICAL)
        executor = smb.ExecOnPosix()
        executor.run("/mnt/share", "sharename2",
                     "192.168.31.13", "username2", "password2")
        mock_getoutput.assert_called_with(
            "sudo mount -t cifs -o rw,uid=username2,username=username2,password=password2,iocharset=utf8 //192.168.31.13/sharename2 /mnt/share"
        )


if __name__ == '__main__':
    unittest.main()
