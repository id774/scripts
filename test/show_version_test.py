#!/usr/bin/env python3

########################################################################
# show_version_test.py: Test for show_version.py
#
#  Description:
#  This script tests show_version.py's behavior when checking module
#  versions and printing the Python version.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-07-07
#       Initial release.
#
#  Test Cases:
#  - Prints Python version with -p
#  - Shows version for known module
#  - Reports module as not found when import fails
#
########################################################################

import io
import os
import sys
import unittest
from contextlib import redirect_stdout
from unittest.mock import MagicMock, patch

# Adjust the path to import script from the parent directory
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import show_version


class TestShowVersion(unittest.TestCase):
    def test_python_version_output(self):
        f = io.StringIO()
        with patch.object(sys, 'argv', ['show_version.py', '-p']):
            with redirect_stdout(f):
                show_version.main()
        output = f.getvalue()
        self.assertIn("Python", output)

    def test_known_module_version(self):
        options = MagicMock()
        options.info = False
        options.python = False
        m = show_version.PythonModuleInfo(options)
        f = io.StringIO()
        with redirect_stdout(f):
            m.get_module_version('math')  # standard module
        output = f.getvalue()
        self.assertIn("math", output)

    def test_missing_module_handling(self):
        options = MagicMock()
        options.info = False
        options.python = False
        m = show_version.PythonModuleInfo(options)

        with patch('importlib.import_module', side_effect=ImportError):
            f = io.StringIO()
            with redirect_stdout(f):
                m.get_module_version('nonexistent_package')
                m.show_not_found()
            output = f.getvalue()
            self.assertIn("nonexistent_package", output)
            self.assertIn("not found", output.lower())


if __name__ == '__main__':
    unittest.main()
